
/*
 * Notes:  Use of the Static Factory method "loadDataFile" is *STRONGLY*
 * recommended over "new DataFile()", since "loadDataFile" deals with
 * duplicated DataFile constructions, registers the DataFile's Tasks,
 * and deals with mapping for RESUME tasks to their counterparts.
 *
 * Note:  loadDataFile() DOES ***NOT*** do loadAttachedFiles(), which loads
 * #using files.  This is considered a compilation-time operation.
 * (And loadAttachedFiles() can throw a CompilationException on problems...)
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

import java.io.File;
import java.util.Enumeration;

public class DataFile extends DataComponent implements DataValidateCode
{
	/* Class Constants */
  public static final String  STANDARD_CXX_HEADER_TAIL= ".H";
  public static final String  STANDARD_CXX_CODE_TAIL  = ".C";
  public static final String  STANDARD_HTML_TAIL      = ".html";
  public static final String  TDL_FILENAME_SUFFIX     = ".tdl";

  public static final String  USING_INDEX             = "U";

  public static final String  DEFAULT_INCLUDES        = "\n#include <tdl.H>\n";

  public static final int     SHOW_USING              = 1100;
  public static final int     HIDE_USING              = 1101;
  public static final int     TRANSLATE_USING         = 1102;



	/* Class Variables */
  protected static boolean   overrideInMessagesFilenamesWithLine = false;
  protected static Registry  fileRegistry = new Registry ( false /*1-to-1*/ );


	/* Class Methods */
  public static boolean getOverrideInMessagesFilenamesWithLine()
		  { return overrideInMessagesFilenamesWithLine; }
  public static void setOverrideInMessagesFilenamesWithLine( boolean theValue )
		      { overrideInMessagesFilenamesWithLine = theValue; }

  protected static Registry getFileRegistry()  { return fileRegistry; }

  public static boolean unregisterFile ( DataFile theDataFile )
  {
	/* ALWAYS: IF we are registered, try to unregister */
    if ( getFileRegistry() . getIsObjectRegistered ( theDataFile ) )
    {
      if ( getFileRegistry() . unregister ( theDataFile ) == false )
      {
	/* Lets be verbose & warn of any potential problems... */
	System.err.println ( "[DataFile:unregisterFile]  Warning:  "
			     + "File was unregistered...  (\""
			     + theDataFile . getFilename() + "\")" );
	return false;
      }
    }
    return true;
  }

  public static boolean registerFile ( DataFile theDataFile )
  {
	/* Trivial case -- don't register these... */
    if ( DataComponent.isEmptyString ( theDataFile . getFilename() ) )
      return true;

	/* Return the results of our attempt to register. */
    if (   getFileRegistry().register( theDataFile.getFilename(), theDataFile )
	== false )
    {
      System.err.println ( "[DataFile:registerFile] Warning:  "
			   + "File registration failed..  (\""
			   + theDataFile.getFilename() + "\")" );
      return false;
    }
    else
      return true;
  }

  public static DataFile getFileForPath ( String theFilename )
  {
    return ( (DataFile)
	     (getFileRegistry() . getFirstObjectForName ( theFilename )) );
  }

  public static String  getNameForFile ( DataFile theDataFile )
  {
    return getFileRegistry() . getNameForObject ( theDataFile );
  }

  public static boolean getIsFileRegistered ( DataFile theDataFile )
  {
    return getFileRegistry() . getIsObjectRegistered ( theDataFile );
  }

  public static boolean getIsFilenameRegistered ( String theFilename )
  {
    return getFileRegistry() . getIsNameRegistered ( theFilename );
  }

  public static Enumeration getRegisteredFiles ( )
  {
    return getFileRegistry() . getObjects();
  }


  public static DataFile  loadDataFile ( File  theFile )
    throws DetailedParseException
  {
	/* Idiocy check.  Have we already loaded this file? */
    if ( DataFile.getIsFilenameRegistered ( theFile.getAbsolutePath() ) )
    {
      System.err.println ( "[DataFile:loadDataFile]  Warning:  "
	   + "Multiple load of file \"" + theFile.getAbsolutePath()
	   + "\"  detected and aborted.  Reusing previous DataFile object." );
      return DataFile.getFileForPath ( theFile.getAbsolutePath() );
    }
	 
	/* Re-Initialize the parser */
    TDLParser . reinitParser ( theFile );

	/* And lets load the file... */
    return privateLoadDataFile ( theFile . getAbsolutePath(),
				 theFile . getName()         );
  }


  public static DataFile  loadDataFile ( String theString )
    throws DetailedParseException
  {
	/* Re-Initialize the parser */
    TDLParser . reinitParser ( theString );

	/* And lets load the file... */
    return privateLoadDataFile ( DataComponent.EMPTY_STRING,
				 DataComponent.EMPTY_STRING );
  }


  protected static DataFile  privateLoadDataFile (
				      String  theFilename,
				      String  theFilenameWithoutPath )
    throws DetailedParseException
  {
    try
    {
	/* Parse the file, creating a new DataFile object. */
      DataFile  dataFile = TDLParser . parseFile ( null );

      dataFile . setFilename ( theFilename );
      dataFile
	. setNullDataSource ( new DataSourceNull ( theFilenameWithoutPath ) );
      dataFile . registerFile ( "DataFile:loadDataFile" );
      dataFile . registerTasks();
      dataFile . establishFilenameBasedUniqueIdentifieresForTasks();
      dataFile . mapResumeTasks();
      return dataFile;
    }
    catch ( Throwable  theExceptionOrError )
    {
      DataComponent.didParseOfSubpartFail ( theExceptionOrError );
    }
	/* We *SHOULD* never get this far.  */
	/* didParseOfSubpartFail() should always throw *SOMETHING* */
    System.err.println ( "[DataFile:loadDataFile]  Warning:  "
			 + "Impossible error has occurred." );
    return null;
  }


  public static String makeTDLFilename ( String theFilename )
  {
    if ( theFilename . endsWith ( DataFile.TDL_FILENAME_SUFFIX ) )
      return theFilename;
    else
      return theFilename + DataFile.TDL_FILENAME_SUFFIX;
  }

  public static String translateTDLFilenameToCxx ( String theFilename,
						   String theSuffix   )
  {
    if ( theFilename . endsWith ( DataFile.TDL_FILENAME_SUFFIX ) )
      return theFilename
	       . substring ( 0,
			     theFilename . length()
			     - DataFile.TDL_FILENAME_SUFFIX . length() )
	+ theSuffix;
    else
      return theFilename;
  }




	/* Instance Variables */
  protected boolean    hasLoadedUsingFiles;
  protected DataVector usingFiles;
  protected String     filename;
  protected DataSource nullDataSource;


	/* Instance Methods */
  public DataFile ( )
  {
    this ( DataComponent.EMPTY_STRING );
  }

  public DataFile ( String theFilename )
  {
    hasLoadedUsingFiles = false;
    usingFiles = new DataVector();
    setFilename ( theFilename );
    setNullDataSource ( null );
  }


  public boolean getIsEmpty()
  {
    return getSubcomponentsCount() <= 0;
  }


  public DataVector getUsingFiles () { return usingFiles; }

  public String getUsingFile ( int theIndex )
  {
    if ( ( theIndex >= 0 )   &&   ( theIndex < getUsingFiles() . count() ) )
      return ( (String)  ( getUsingFiles() . elementAt ( theIndex ) ) );
    else
    {
      System.err.println ( "[DataFile:getUsingFile]  Warning:  "
			   + "Illegal index (" + theIndex + ")." );
      return null;
    }
  }


  public void addUsingFile ( String theUsingFile )
  {
    if (   ( theUsingFile . charAt ( 0 ) != '\"' )
	&& ( theUsingFile . charAt ( 0 ) != '<'  ) )
    {
      System.err.println ( "[DataFile:addUsingFile]  Warning:  "
			   + "Invalid first character of string (\""
			   + theUsingFile . charAt ( 0 ) + "\")." );
    }

    if (   ( theUsingFile . charAt ( theUsingFile . length() - 1 ) != '\"' )
	&& ( theUsingFile . charAt ( theUsingFile . length() - 1 ) != '>'  ) )
    {
      System.err.println ( "[DataFile:addUsingFile]  Warning:  "
		   + "Invalid last character of string (\""
		   + theUsingFile . charAt ( theUsingFile . length() - 1  )
		   + "\")." );
    }

	/* Store the new <file> or "file" */
    getUsingFiles() . addElement ( theUsingFile );
  }


  public void addUsingFileAndStripUsing ( String theUsingFile )
  {
    int  index = 0;

    if ( theUsingFile . charAt ( index ) == '#' )
      index ++;

    while (   ( theUsingFile . charAt ( index ) == ' '  )
	   || ( theUsingFile . charAt ( index ) == '\t' ) )
      index ++;

    if (   ( theUsingFile . charAt ( index ) == 'u' )
	|| ( theUsingFile . charAt ( index ) == 'U' ) )
      index ++;

    if (   ( theUsingFile . charAt ( index ) == 's' )
	|| ( theUsingFile . charAt ( index ) == 'S' ) )
      index ++;

    if (   ( theUsingFile . charAt ( index ) == 'i' )
	|| ( theUsingFile . charAt ( index ) == 'I' ) )
      index ++;

    if (   ( theUsingFile . charAt ( index ) == 'n' )
	|| ( theUsingFile . charAt ( index ) == 'N' ) )
      index ++;

    if (   ( theUsingFile . charAt ( index ) == 'g' )
	|| ( theUsingFile . charAt ( index ) == 'G' ) )
      index ++;

    while (   ( theUsingFile . charAt ( index ) == ' '  )
	   || ( theUsingFile . charAt ( index ) == '\t' ) )
      index ++;

    addUsingFile ( theUsingFile . substring ( index ) );
  }




  public String getFilename()  { return filename; }
  public void   setFilename ( String theFilename )
  {
    filename = theFilename;
  }


  public DataSource getNullDataSource()  { return nullDataSource; }
  public void       setNullDataSource ( DataSource theDataSource )
			        { nullDataSource = theDataSource; }

  public String getMessageFilenameLead()
  {
	/* Use a DataSource, ideally a DataSourceNull object, to keep this *
	 * code -- the "Line:" vs "Filename:" code -- all in one place.    */
    if ( getNullDataSource() == null )
      return DataComponent.DEFAULT_MESSAGE_FILENAME_LEAD;
    else
      return getNullDataSource() . getFilenameWithoutPathPlusColon();
  }



      /* Convenience Method -- This should *ALMOST* never need to be called.*/
  public boolean registerFile ( String theErrorLocation )
  {
    if ( DataFile.registerFile ( this ) == false )
    {
      System.err.println ( "[DataFile:registerFile] --  "
			   + "[\"" + theErrorLocation + "\"] --  "
			   + "Error:  New Registration of file has failed. (\""
			   + getFilename() + "\")." );
      return false;
    }
    else
      return true;
  }
      /* Convenience Method -- This should *ALMOST* never need to be called.*/
  public boolean unregisterFile ( String theErrorLocation )
  {
    if ( DataFile.unregisterFile ( this ) == false )
    {
      System.err.println ( "[DataFile:unregisterFile] --  "
		     + "[\"" + theErrorLocation + "\"] --  "
		     + "Warning:  unregistration of file has failed.  (\""
			   + getFilename() + "\")." );
      return false;
    }
    else
      return true;
  }
      /* Convenience Method */
  public boolean getIsFileRegistered()
  {
    return DataFile.getIsFileRegistered ( this );
  }

     /* Grunt method to go through and register all the tasks inside this
      * DataFile.  Should NEVER be called more than once per file!
      * Invoked by loadDataFile().
      */
  protected boolean registerTasks ( )
    throws DetailedParseException 
  {
    boolean returnValue = true;

    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
		/* If this is a DataTaskDefinition Object */
      if (   ( isSubcomponentADataComponent ( i ) )
	  && ( getDataComponentSubcomponent ( i )
	       instanceof DataTaskDefinition      )
		/* Do *NOT* try to register resume tasks */
	  && ( ( (DataTaskDefinition) getDataComponentSubcomponent ( i ) )
	           . getTaskType() != DataTaskDefinition.RESUME_TASK )
	  )
      {
	if (   ( (DataTaskDefinition) getDataComponentSubcomponent ( i ) )
	         . registerTask ( "DataFile:registerTasks", true )
	     == false )
	{
	  returnValue = false;
	}
      }
    }

    return returnValue;
  }

	/* We want to replace the default unique random-numbers generated
	 * by DataTaskDefinition with "_<filename>_<number>".  To facilitate
	 * this, we use a hashtable, creating a recurisive key/value pairing
	 * along the lines of name -> object -> object -> object -> object.
	 */
  protected void establishFilenameBasedUniqueIdentifieresForTasks ( )
  {
    int                 i, taskCount;
    DataTaskDefinition  currentTask;
    Object              oldTask, storeTaskUnderObject;
    DataHashtable       tasksHashtable      = new DataHashtable();
    String              filenameSegmentOfUniqueId;

    filenameSegmentOfUniqueId
      =   "_" + getFilenameBasedUniqueIdentifierString ( false, false, false,
							 null )
        + "_";

    for ( i=0;  i < getSubcomponentsCount();  i++ )
    {
		/* If this is a DataTaskDefinition Object */
      if (   ( isSubcomponentADataComponent ( i ) )
	  && ( getDataComponentSubcomponent ( i )
	       instanceof DataTaskDefinition      )
		/* Do *NOT* bother with Resume Tasks -- they will use  *
		 * their master's unique-id string when they need one, *
		 * and would just confuse this name-based indexing...  */
	  && ( ( (DataTaskDefinition) getDataComponentSubcomponent ( i ) )
	           . getTaskType() != DataTaskDefinition.RESUME_TASK )
	  )
      {
	currentTask = (DataTaskDefinition) getDataComponentSubcomponent ( i );

	  /* In case there are zero tasks so far -- store under name. */
	storeTaskUnderObject = currentTask . getTaskScopeAndName();

	for ( taskCount = 0,
	      oldTask   = tasksHashtable
			    . get ( currentTask . getTaskScopeAndName() )
		;
	      oldTask  != null
		;
	      taskCount ++,
	      oldTask   = tasksHashtable . get ( oldTask )
	    )
	{
	  storeTaskUnderObject = oldTask;
	}

	tasksHashtable . put ( storeTaskUnderObject, currentTask );
	currentTask . setUniqueIdString ( filenameSegmentOfUniqueId
					  + taskCount );
      } /* IF ( subcomponent [ i ] is a DataTaskDefinition ) */
    }
  }


     /* Grunt method to go through and map all RESUME tasks to their
      * counterparts inside this DataFile.
      * Must be invoked after registerTasks().
      * Invoked automatically by loadDataFile().
      */
  protected void mapResumeTasks ( )
    throws DetailedParseException
  {
    DataHashtable      tasksHashtable = new DataHashtable();
    DataTaskDefinition counterpartDataTaskDefinition,
                       resumeDataTaskDefinition,
                       currentDataTaskDefinition;

	/* Clear all current resume-task mappings */
    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
		/* If this is a DataTaskDefinition Object */
      if (   ( isSubcomponentADataComponent ( i ) )
	  && ( getDataComponentSubcomponent ( i )
	       instanceof DataTaskDefinition      ) )
      {
	( (DataTaskDefinition) getDataComponentSubcomponent ( i ) )
	    . clearResumeTasksVector();
	( (DataTaskDefinition) getDataComponentSubcomponent ( i ) )
	    . setResumeMasterTask ( null );
      }
    }

	/* Set the current resume-task mappings. */
    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
		/* If this is a DataTaskDefinition Object */
      if (   ( isSubcomponentADataComponent ( i ) )
	  && ( getDataComponentSubcomponent ( i )
	       instanceof DataTaskDefinition      ) )
      {
	currentDataTaskDefinition
	  = (DataTaskDefinition) getDataComponentSubcomponent ( i );

	  /* Not much we can do with empty task-names... */
	if ( DataComponent.isEmptyString ( currentDataTaskDefinition
					     . getTaskName()         ) )
	{
	  System.err.println ( "[DataFile:mapResumeTasks]  Warning: "
			       + "Null task-name encountered.  Skipping..." );
	  continue;
	}

	if (    currentDataTaskDefinition . getTaskType()
	     != DataTaskDefinition.RESUME_TASK            )
	{
	  tasksHashtable . put ( currentDataTaskDefinition . getTaskName(),
				 currentDataTaskDefinition );
	}
	else
	{
	  resumeDataTaskDefinition = currentDataTaskDefinition;

		/* This gives us the last task defined with the same name. */
	  counterpartDataTaskDefinition
	    = (DataTaskDefinition)
	      tasksHashtable . get( resumeDataTaskDefinition . getTaskName() );

		/* Try checking for Resume before first Task definition. */
	  if ( counterpartDataTaskDefinition == null )
	  {
	    counterpartDataTaskDefinition
	      = DataTaskDefinition.getFirstTaskForName ( 
				    resumeDataTaskDefinition . getTaskName() );
	  }


	    /* Idiocy check:  No corresponding Task */
	  if (   ( counterpartDataTaskDefinition            == null  )
	      && ( resumeDataTaskDefinition . getIsExtern() == false ) )
	  {
	    throw new DetailedParseException (
		        resumeDataTaskDefinition . getMessageFilenameLead()
		      + resumeDataTaskDefinition . getLineNumberString()
		      + ":  Error:  Resume Task named \""
		      + resumeDataTaskDefinition . getTaskName()
		      + "\" has no corresponding counterpart." );
	  }


	  if ( counterpartDataTaskDefinition != null )
	  {
	      /* Idiocy check:  Can't resume an EXCEPTION. */
	      /*                (Though you can resume an EXCEPTION HANDLER! */
	    if (    counterpartDataTaskDefinition . getTaskType()
		 == DataTaskDefinition.EXCEPTION_TASK )
	    {
	      throw new DetailedParseException (
		        resumeDataTaskDefinition . getMessageFilenameLead()
		      + resumeDataTaskDefinition . getLineNumberString()
		      + ":  Error:  Resume Task named \""
		      + resumeDataTaskDefinition . getTaskName()
		      + "\" may not resume an EXCEPTION Task." );
	    }


	      /* Establish this resume-task mapping. */
	    counterpartDataTaskDefinition
	      . addResumeTask ( resumeDataTaskDefinition );

	    resumeDataTaskDefinition
	      . setResumeMasterTask ( counterpartDataTaskDefinition );

	  } /* if ( counterpartDataTaskDefinition != null ) */
	} /* IF ( this is NOT a resume Task ) ...  ELSE ... */
      } /* IF ( subcomponent [ i ] is a DataTaskDefinition ) */
    } /* for ( int i=0;  i < getSubcomponentsCount();  i++ ) */
  } /* protected void mapResumeTasks() throws DetailedParseException */




	/* Dirs ==> Directories to Search for #using""  and #using<> */
  public void loadAttachedFiles ( DataVector theUsingQuoteDirs,
				  DataVector theUsingAngleBracketDirs,
				  boolean    theShouldBeVerbose     )
    throws CompilationException
  {
    int        i, j;
    DataVector dirsToSearch = null;
    File       directory, file;
    String     usingFilename;
    DataFile   dataFile;


	/* If we have already loaded, or begun loading this file's #using */
	/* statements, abort now!  Don't start an endless recursive loop! */
    if ( hasLoadedUsingFiles )
    {
      if ( theShouldBeVerbose )
	System.err.println ( "#using files are already loaded for file \""
			     + getFilename() + "\"." );
      return;
    }

    if ( theShouldBeVerbose )
      System.err.println ( "Loading #using files for \""
			   + getFilename() + "\".   ( Memory = "
			   + Runtime.getRuntime().freeMemory() + " / "
			   + Runtime.getRuntime().totalMemory() + " )." );



	/* Note that we have succeeded, or at least begun loading files */
	/* to kill endless recursive looping... */
    hasLoadedUsingFiles = true;


    for ( i=0;  i < getUsingFiles().count();  i++ )
    {
	/* Find our using filename */
      usingFilename = getUsingFile ( i )
	                . substring ( 1,
				      getUsingFile ( i ) . length() - 1 );

	/* Find where we are searching... */
      switch ( getUsingFile ( i ) . charAt ( 0 ) )
      {
	case '\"':
	  dirsToSearch = theUsingQuoteDirs;
	  break;

	case '<':
	  dirsToSearch = theUsingAngleBracketDirs;
	  break;

	default:
	  dirsToSearch = null;
	  System.err.println ( "[DataFile:loadAttachedFiles]  Warning:  "
		       + "Unable to load file for invalid statement:  #using "
		       + getUsingFile ( i ) );
	  break;

      } /* switch ( getUsingFile ( i ) . charAt ( 0 ) ) */


	/* Search through our list of possible directories... */
      for ( j=0;  (dirsToSearch != null) && (j < dirsToSearch.count());  j++ )
      {
	directory = (File) ( dirsToSearch . elementAt ( j ) );

	if ( directory . isDirectory() == false )
	{
	  System.err.println ( "[DataFile:loadAttachedFiles]  Warning:  "
			       + "Bad search directory:  \""
			       + directory.getAbsolutePath() + "\"." );
	  continue;
	}

	file = new File ( directory, usingFilename );

	if ( file . exists() )
	{
	  if ( DataFile.getIsFilenameRegistered ( file . getAbsolutePath() )
	       == false )
	  {
	    try
	    {
	      if ( theShouldBeVerbose )
		System.err.println ( "Loading File \""
				     + file . getAbsolutePath() + "\"." );

		/* Load and register this file */
	      DataFile . loadDataFile ( file );
	    }
	    catch ( DetailedParseException  theException )
	    {
	      throw new CompilationException (
			      "Error:  Encountered Parse Exception in file \""
			      + file . getAbsolutePath()
			      + "\", which was #using'ed from file \""
			      + getFilename() + "\":\n"
			      + theException . toString() );
	    }
	  } /* IF ( file is NOT already loaded/registered ) */
	  else
	  {
	    if ( theShouldBeVerbose )
	      System.err.println ( "File \"" + file . getAbsolutePath()
				   + "\" was already loaded." );
	  }

		/* Find this dataFile */
	  dataFile = DataFile.getFileForPath ( file . getAbsolutePath() );

		/* Did the impossible just happen? */
	  if ( dataFile == null )
	  {
	    System.err.println ( "[DataFile:loadAttachedFiles]  Warning:  "
	       + "Programmatical Error.  Unable to load #using files for \""
	       + file . getAbsolutePath() + "\"." );
	  }
	  else
	  {
		/* Load any files it has attached.  (Chain #using) */
	    dataFile . loadAttachedFiles ( theUsingQuoteDirs,
					   theUsingAngleBracketDirs,
					   theShouldBeVerbose );
	  }

	    /* Found a match.  Abort Search. */
	  break;

	} /* if ( file . exists() ) */
			  
      } /* FOR ( 0 <= j < dirsToSearch.count() ) */

	/* If we didn't break out of that FOR ( j ) loop... */
      if ( ( dirsToSearch == null )   ||   ( j >= dirsToSearch.count() ) )
      {
	throw new CompilationException (
		      "Error:  Unable to locate #using file \"" + usingFilename
		      + "\".  (Which was #using'ed from file \""
		      + getFilename() + "\")" );
      }

    } /* for ( i=0;  i < getUsingFiles().count();  i++ ) */


    if ( theShouldBeVerbose )
      System.err.println ( "..Finished Loading #using files for \""
			   + getFilename() + "\".   ( Memory = "
			   + Runtime.getRuntime().freeMemory() + " / "
			   + Runtime.getRuntime().totalMemory() + " )." );

  } /* public void loadAttachedFiles ( ... ) */



  public void validateExternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
	/* Check subcomponents */
    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
      if (   ( isSubcomponentADataComponent ( i )                             )
	  && ( getDataComponentSubcomponent ( i ) instanceof DataValidateCode )
	  )
      {
	( (DataValidateCode) (getDataComponentSubcomponent ( i )) )
	  . validateExternalCode ( theReference, theReturnValue );
      }
    }
  }


  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
    throw new CompilationException (
	     "Internal Error:  DataFile object may not exist inside a Task." );
  }



  public String getWarnString ( int theObjectSubset )
  {
    return super . getWarnString ( theObjectSubset )
      + " or DataComponent.HTML_DOCUMENTATION ("
      + DataComponent.HTML_DOCUMENTATION + ")";
  }


  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    return (   ( theObjectSubset == DataComponent.HTML_DOCUMENTATION )
	    || ( super . isValidObjectSubset ( theObjectSubset )     ) );
  }



  public String getFilenameBasedUniqueIdentifierString (
				  boolean thePrependCxxNameLead,
				  boolean theUseSemiRandomNumbersForDefault,
				  boolean theUseTranslatedTdlFilename,
				  String messageFilenameLead )
  {
    StringBuffer stringBuffer;
    String       prependString;
    String       filenameString;

    if (messageFilenameLead == null) 
      messageFilenameLead = getMessageFilenameLead();
    else // To be consistent with what getMessageFilenameLead does
      messageFilenameLead += ":";

    if ( thePrependCxxNameLead )
      prependString = DataComponent.CXX_NAME_LEAD;
    else
      prependString = "";


    if (   messageFilenameLead . equals (
			    DataComponent.DEFAULT_MESSAGE_FILENAME_LEAD       )
	|| messageFilenameLead . equals (
			    DataComponent.DEFAULT_DATA_SOURCE_NAME_WITH_COLON )
        )
    {
      if ( theUseSemiRandomNumbersForDefault )
	  /* getUniqueIdentifierString(String) Adds unique (Hex) suffix */
	return getUniqueIdentifierString ( prependString + "STDIN:" );
      else
	  /* getUniqueIdentifierString(StringBuffer) removes illegal chars */
	return getUniqueIdentifierString (
		 new StringBuffer ( prependString + "STDIN" ) );
    }
    else
    {
      stringBuffer = new StringBuffer ( prependString );
		     /* getMessageFilenameLead() gets rid of any path  *
		      * separator characters, but adds a trailing ":". */
      filenameString = messageFilenameLead . substring (
			 0,
			 messageFilenameLead . length() - 1 );

      if ( theUseTranslatedTdlFilename )
	filenameString = DataFile.translateTDLFilenameToCxx (
			   filenameString, DataFile.STANDARD_CXX_HEADER_TAIL );

      stringBuffer . append ( filenameString );

	  /* getUniqueIdentifierString(StringBuffer) removes illegal chars */
      return getUniqueIdentifierString ( stringBuffer );
    }
  }

  public String generateBeginningOfFile (
				   DataDestination  theOutputDestination,
				   int              theObjectSubsetToGenerate )
  {
    String ifdefString = null;

    if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) == true )
    {
      ifdefString = getFilenameBasedUniqueIdentifierString( true, true, true,
				theOutputDestination.getCxxFileName() );

      theOutputDestination . write ( "\n#ifndef " );
      theOutputDestination . write ( ifdefString );
      theOutputDestination . write ( "\n#define " );
      theOutputDestination . write ( ifdefString );
      theOutputDestination . write ( "\n" );


      theOutputDestination . write ( DataFile.DEFAULT_INCLUDES );
      theOutputDestination . write ( "\n" ); 
    } /* if ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) == true ) */

    return ifdefString;
  }


  public void generateEndingOfFile ( DataDestination  theOutputDestination,
				     String           theIfdefString       )
  {
    if ( theIfdefString != null )
    {
      theOutputDestination . write ( "\n\n#endif /*" );
      theOutputDestination . write ( theIfdefString );
      theOutputDestination . write ( "*/\n\n" );
    }
  }



  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
      generate ( theOutputDestination, theObjectSubsetToGenerate,
		 TDLC.NO_SUBSET, DataFile.SHOW_USING, true, true, true );
  }

	/* Unfortunately, I need theObjectSubsetToGenerate to determine
	 * how this DataFile's contained objects should print, so I can't
	 * use my usual tricks with theObjectSubsetToGenerate to decide
	 * how to generate this DataFile.  However, since DataFile's are
	 * top-level objects, I *CAN* have multiple non-standard generate()
	 * methods...
	 */
  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate,
			 int              theUsingStatementGenerationMethod,
			 int              theDistributedExceptionsMethod,
			 boolean          theShowExternTasks,
                         boolean          theShowNonTaskCode,
			 boolean          theShowClassCode )
  {
    String              ifdefString;
    int                 subcomponentIndex, maxIndex;
    DataComponent       dataComponent;
    DataTaskDefinition  dataTask;
    int                 insideClassCount = 0;


       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* Are we just doing HTML generation? */
    if ( theObjectSubsetToGenerate == DataComponent.HTML_DOCUMENTATION )
    {
      generateHTML ( theOutputDestination, true );
      return;
    }


	/* If necessary start us off with a #ifdef... */
    ifdefString = generateBeginningOfFile ( theOutputDestination,
					    theObjectSubsetToGenerate );


	/* Start us off with the first subcomponent */
    subcomponentIndex = 0;


	/* For each #using, generate leading subcomponents and then the #using.
         * Note: Do this loop n+1 times to get any trailing subcomponents.
	 */
    for ( int i=0;  i <= getUsingFiles() . count();  i++ )
    {
	/* For the first N iterations... */
      if ( i < getUsingFiles() . count() )
      {    /* Use the stored subcomponent index. */
	maxIndex = getIndex ( DataFile.USING_INDEX + i );
      }
      else
      {    /* For the last iteration, generate all remaining subcomponents. */
	maxIndex = getSubcomponentsCount();
      }


	/* Print the leading subcomponents... */
      for ( ; subcomponentIndex < maxIndex; subcomponentIndex ++ )
      {
	if ( isSubcomponentAString ( subcomponentIndex ) )
	{
	  if (      theShowNonTaskCode
	       || ( theShowClassCode && ( insideClassCount > 0 ) ) )
	  {
	    theOutputDestination
	      . write ( getStringSubcomponent ( subcomponentIndex ) );
	  } /* If ( subComponent is a String ) */
	}

	else if ( isSubcomponentADataComponent ( subcomponentIndex ) )
	{
	  dataComponent = getDataComponentSubcomponent ( subcomponentIndex );

	  if ( dataComponent instanceof DataClassStructNamespace )
	  {
	    if ( ((DataClassStructNamespace) dataComponent) . getIsStart() )
	      insideClassCount ++;
	    else
	      insideClassCount --;

	    if ( theShowClassCode )
	      dataComponent . generate ( theOutputDestination,
					 theObjectSubsetToGenerate );
	  }


		/* Either: This is a DataTaskDefinition,
		 *         AND we are showing extern tasks
		 *             OR we have a non-extern task,
		 *         OR  This is NOT a DataTaskDefinition,
		 *         AND  we are showing non-task code
                 *                AND we are outside of a class.
		 *           OR we are showing class code
		 *                AND we are inside a class.
		 */
	  else if (
	    (  (   ( dataComponent instanceof DataTaskDefinition )
		&& (      theShowExternTasks
		     || ( ((DataTaskDefinition) dataComponent)
			     . getIsExtern() == false ) ) )
	    || (   ( false == (dataComponent instanceof DataTaskDefinition) )
		&& (   ( theShowNonTaskCode && ( insideClassCount <= 0 ) )
		    || ( theShowClassCode   && ( insideClassCount >  0 ) )  ) )
	    ) )
	  {
	    if ( dataComponent instanceof DataTaskDefinition )
	      ((DataTaskDefinition)dataComponent)
		  . generate ( theOutputDestination,
			       theObjectSubsetToGenerate,
			       theDistributedExceptionsMethod,
			       theShowClassCode );
	    else
	      dataComponent . generate ( theOutputDestination,
					 theObjectSubsetToGenerate );
	  }

		/* DataExternH, DataDistributedTypedef, DataDistributedStruct,
		 * and DataDistributedEnum are special cases that need to show
		 * up in the Header files.  They know how to deal with
		 * themselves (show/don't show non-task code) internally
		 * based on theObjectSubsetToGenerate.
		 */
	  else if (   ( dataComponent instanceof DataExternH            )
		   || ( dataComponent instanceof DataDistributedTypedef )
		   || ( dataComponent instanceof DataDistributedStruct  )
		   || ( dataComponent instanceof DataDistributedEnum    ) )
	  {
	      dataComponent . generate ( theOutputDestination,
					 theObjectSubsetToGenerate );
	  }
	} /* If ( subComponent is a DataComponent) */

	else
	{
	  System.err.println ( "[DataFile:generate]  Warning:  "
			       + "Unrecognized subcomponent \""
			       + getSubcomponent ( subcomponentIndex )
			           . getClass() . getName()
			       + "\".  Skipping..." );
	}

      } /* FOR ( subcomponentIndex < getIndex ( DataFile.USING_INDEX + i ) ) */



	/* If we have a #using statement */
      if ( i < getUsingFiles() . count() )
      {
	  /* Display the Using statement in the appropriate way... */
	switch ( theUsingStatementGenerationMethod )
	{
	  case DataFile.SHOW_USING:
	    if ( theOutputDestination . getColumn() != 0 )
	      theOutputDestination . write ( "\n" );
	    theOutputDestination . write ( "# using " );
	    theOutputDestination . write ( getUsingFile ( i ) );
	    if ( theShowNonTaskCode == false )
	      theOutputDestination . write ( "\n" );
	    break;

	  case DataFile.HIDE_USING:
	    break;

	  case DataFile.TRANSLATE_USING:
	    if ( theOutputDestination . getColumn() != 0 )
	      theOutputDestination . write ( "\n" );
	    theOutputDestination . write ( "#include " );
	    theOutputDestination . write ( getUsingFile ( i )
					     . substring ( 0, 1 ) );
	    theOutputDestination
	      . write ( DataFile.translateTDLFilenameToCxx (
			      getUsingFile ( i ) . substring (
				           1,
				           getUsingFile ( i ) . length() - 1 ),
			      DataFile.STANDARD_CXX_HEADER_TAIL ) );
	    theOutputDestination
	      . write ( getUsingFile ( i )
		          . substring ( getUsingFile ( i ) . length() - 1,
					getUsingFile ( i ) . length()     ) );
	    if ( theShowNonTaskCode == false )
	      theOutputDestination . write ( "\n" );
	    break;
	} /* switch ( theUsingStatementGenerationMethod ) */

      } /* if ( i < getUsingFiles() . count() ) */

    } /* FOR ( 0 <= i <= getUsingFiles() . count() ) */


	/* Conclude any #ifdef that we may have started... */
    generateEndingOfFile ( theOutputDestination, ifdefString );
  }



  public void generateHTMLHeader ( DataDestination  theOutputDestination )
  {
    generateHTMLHeader ( theOutputDestination, null );
  }

  public void generateHTMLHeader ( DataDestination  theOutputDestination,
				   String           theTitle             )
  {
    theOutputDestination . write ( "<HTML>\n<HEAD>\n<TITLE>" );

    if ( theTitle != null )
    {
      theOutputDestination . write ( theTitle );
    }

    else if ( getFilename() != null )
    {
      theOutputDestination . write ( "File: " );
      theOutputDestination . write ( getFilename() );
    }

    else
    {
      theOutputDestination . write ( "Untitled" );
    }

    theOutputDestination . write ( "</TITLE>\n</HEAD>\n<BODY>\n" );
  }


  public void generateHTMLTrailer ( DataDestination  theOutputDestination )
  {
    theOutputDestination . write ( "\n</BODY>\n</HTML>\n" );
  }


  public void generateHTML ( DataDestination  theOutputDestination,
			     boolean          theIsDoingEntireHTMLFile )
  {
    if ( theIsDoingEntireHTMLFile )
      generateHTMLHeader ( theOutputDestination );


	/* Write title of section... */
    if ( theIsDoingEntireHTMLFile == false )
    {
      theOutputDestination . write ( "<BR><BR><HR>\n" );
    }
    theOutputDestination . write ( "<H1 ALIGN=CENTER>" );
    theOutputDestination . write ( "File: " );
    theOutputDestination . write ( getFilename() );
    theOutputDestination . write ( "</H1><HR><BR>\n" );

	/* Generate tasks... */
    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
      if (   ( isSubcomponentADataComponent ( i ) )
	  && ( getDataComponentSubcomponent ( i )
	       instanceof DataTaskDefinition      ) )
      {
	( (DataTaskDefinition) (getDataComponentSubcomponent ( i )) )
	  . generate ( theOutputDestination,
		       DataComponent.HTML_DOCUMENTATION );
      }
    }

    if ( theIsDoingEntireHTMLFile )
      generateHTMLTrailer ( theOutputDestination );
  }
}


