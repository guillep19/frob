
/*
 * Known BUGS:
 *  - There *should* be a default, implicit searching of the current
 *    directory for #using"" statements (which is not currently happening).
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */


import java.io.IOException;
import java.io.File;
import java.text.DateFormat;
import java.util.TimeZone;
import java.util.Date;


public class TDLC extends Object
{
	/* Version: */
  public final static String  MAJOR_VERSION_NUMBER     = "1";
  public final static String  MINOR_VERSION_NUMBER     = "5";
  public final static String  MICRO_VERSION_NUMBER     = "3";
  public final static String  VERSION_NUMBER =       TDLC.MAJOR_VERSION_NUMBER
					     + "." + TDLC.MINOR_VERSION_NUMBER
					     + "." + TDLC.MICRO_VERSION_NUMBER;

	/* Global Variables.
	 * (They should never be used, but occasionally they prove useful
	 *  in intermediate releases.)
	 */
  public final static boolean DEBUG_ENABLED = false;


	/* Class Constants */
  public final static char    UNSET_VALUE                 = ' ';
  public final static int     NO_SUBSET                   = 0;
  public final static int     GENERATE_HEADER_ONLY        = -1;

  public final static char    ARG_HELP                    = 'h';
  public final static char    ARG_VERSION                 = 'V';
  public final static char    ARG_DEBUG                   = 'n';
  public final static char    ARG_VERBOSE                 = 'v';
  public final static char    ARG_YES                     = 'y';

  public final static char    ARG_DISABLE_LINE_MACROS     = 'L';
  public final static char    ARG_USE_LINE_NOT_FILENAME   = 'l';

  public final static char    ARG_ENABLE_VALIDATION       = 'W';
  public final static char    ARG_DISABLE_VALIDATION      = 'Q';

  public final static char    ARG_INCLUDE_CLASSES         = 'c';
  public final static char    ARG_REMOVE_CLASSES          = 'R';

  public final static char    ARG_STANDARD_USING          = 'u';
  public final static char    ARG_NO_USING                = 'N';
  public final static char    ARG_TRANLATE_USING          = 't';

  public final static char    ARG_DELETE_EXTERN_TASKS     = 'd';
  public final static char    ARG_ENABLE_EXTERN_TASKS     = 'e';
  public final static char    ARG_DELETE_NON_TASK_CODE    = 'D';
  public final static char    ARG_ENABLE_NON_TASK_CODE    = 'E';

  public final static char    ARG_FULL_INLINE_TYPE        = '1';
  public final static char    ARG_FULL_EXTERN_TYPE        = '2';
  public final static char    ARG_HIDE_EXTERN_TYPE        = '3';
  public final static char    ARG_COMBINED_EXTERN_TYPE    = '4';
  public final static char    ARG_DISTRIBUTED_ONLY_INLINE = '5';
  public final static char    ARG_DISTRIBUTED_ONLY_EXTERN = '6';

  public final static char    ARG_HTML                    = 'm';
  public final static char    ARG_SMART_ANALYZER          = 'a';
  public final static char    ARG_INLINED_CXX             = 'i';
  public final static char    ARG_HEADER_CXX              = 'H';
  public final static char    ARG_CODE_CXX                = 'C';
  public final static char    ARG_STANDARD_CXX            = 'S';

  public final static char    ARG_LOAD_USING              = 's';
  public final static char    ARG_INCLUDE_DIR             = 'I';
  public final static char    ARG_USING_DIR               = 'U';

  public final static char    ARG_USE_OLD_ROOT_NODE_VALUE = 'r';

  public final static char    ARG_ADD_TEXT                = 'A';

  public final static char    ARG_OUT_FILE                = 'o';

  public final static char    ARG_DISTRIBUTED_EXCEPS_ONLY = 'x';
  public final static char    ARG_DISTRIBUTED_EXCEPS_NONE = 'X';

  public final static String  USAGE_STRING
="Usage:  java TDLC [-hVnvyLlWQuNtedED123456maiHCSsr] "
                           + "[-{IU} <dir>] [-A <text>] [-o <file>] [<file>]\n"
+"  -h         -- Just show this usage list.\n"
+"  -V         -- Just show the current version. [" +TDLC.VERSION_NUMBER +"]\n"
+"  -n         -- Show what would happen without actually doing it.\n"
+"  -v         -- Be verbose about what stage we are in.\n"
+"  -y         -- Auto-answer \"Yes\" when questioned about overwriting "
								   + "files.\n"
+"\n"
+"  -L         -- Disable #line macros.\n"
+"  -l         -- Use \"Line:\" instead of \"<filename>:\" to start with\n"
+"                when printing TDLC-generated error/warning messages.\n"
+"\n"
+"  -W         -- Issue warnings/errors about questionable code. (Default)\n"
+"  -Q         -- Be quiet.  Do NOT check for questionable code.\n"
+"\n"
+"  -c         -- Copy   classes/structs/namespaces from source to output.\n"
+"  -R         -- Remove classes/structs/namespaces from source in output.\n"
+"\n"
+"  -u         -- Normal.  #using statements are printed as they appear.\n"
+"  -N         -- Strip out all #using statements.\n"
+"                  (Default for Code files.)\n"
+"  -t         -- Translate #using statements to #include statements.\n"
+"                  (Default for Header files.)\n"
+"                  Note: The #using<> filenames are translated to their\n"
+"                        (assumed) C++ counterpart names.  (Ie: A \""
                              + DataFile.STANDARD_CXX_HEADER_TAIL + "\" is\n"
+"                        appended, after first removing any trailing \".tdl"
                                                                     + "\".)\n"
+"\n"
+"          Extern-Tasks merely generate their function-interface.\n"
+"  -e         -- Enable extern-tasks in   the output.  (Default)\n"
+"  -d         -- Delete extern-tasks from the output.\n"
+"\n"
+"  -E         -- Enable non-task code in   the output. (Default for code.)\n"
+"  -D         -- Delete non-task code from the output. (Default for headers.)"
                                                                          +"\n"
+"\n"
+"          These next options refine what the -H, -C, and -S flags produce.\n"
+"          (They also change the defaults for the #using flags -[uNt]\n"
+"           and include/remove classes/structs/namespaces flags -[cR].)\n"
+"  -1         -- Created Class declaration in header,  inline functions.\n"
+"                  (Header: Translate #using [t],  Code: Strip #using. [N])\n"
+"                  (Header: Include classes  [c],  Code: Remove classes [R].)"
                                                                          +"\n"
+"  -2         -- Created Class declaration in header,  extern functions.\n"
+"                  (Header: Translate #using [t],  Code: Strip #using. [N])\n"
+"                  (Header: Include classes  [c],  Code: Remove classes [R].)"
                                                                          +"\n"
+"  -3         -- Created Class declaration in code,    extern functions.\n"
+"                  (Header: Strip #using [N],  Code: Translate #using. [t])\n"
+"                  (Header: Remove classes [R], Code: Include classes [c].)"
                                                                          +"\n"
+"  -4         -- Created Class declaration in code,    extern functions.\n"
+"                  (Header: Strip #using [N],  Code: Translate #using. [t])\n"
+"                  (Header: Remove classes [R], Code: Include classes [c].)"
                                                                          +"\n"
+"                  (Header: Equivalent to -3H; Code: Equivalent to -2H, -2C.)"
+"\n"
+"  -5         -- Distributed-Only Invocation generation, inline functions.\n"
+"                  (Meaningful only with -5H.  -5C produces an empty file.)\n"
+"  -6         -- Distributed-Only Invocation generation, EXTERN functions.\n"
+"                  (Usable with -6H, -6C, -6S.)\n"
+"  -x         -- Output code only related to distributed exceptions\n"
+"  -X         -- Do not output any code related to distributed exceptions\n"
+"\n"
+"  -m         -- Generate HTML Documentation.\n"
+"  -a         -- Generate SMv AnalyzeR for Tca (SMART) translation.\n"
+"  -i         -- Generate Inlined C++ Header code.\n"
+"  -H         -- Generate C++ Header code.\n"
+"  -C         -- Generate C++ code.\n"
+"  -S         -- Generate both (Standard) C++ "
			      + DataFile.STANDARD_CXX_CODE_TAIL   + " and "
			      + DataFile.STANDARD_CXX_HEADER_TAIL + " files.\n"
+"                 Stores output in sourceFile"
			      + DataFile.STANDARD_CXX_CODE_TAIL
                              + " and sourceFile"
			      + DataFile.STANDARD_CXX_HEADER_TAIL + ".\n"
+"                 If \"-o\" is also used, the \"-o file\" replaces "
							      + "sourceFile.\n"
+"                 (If sourceFile matches *.tdl, the \".tdl\" is removed.)\n"
+"                 And \"#include <sourceFile"
                               + DataFile.STANDARD_CXX_HEADER_TAIL
                               + ">\" is added to the code file.\n"
+"\n"
+"  -s         -- Load any #using files.\n"
+"  -I <file>  -- Specify search directory for #using<> and #using\"\".\n"
+"  -U <file>  -- Specify search directory for #using<> only.\n"
+"\n"
+"  -r         -- Use \"::" + DataComponent.CXX_ENCLOSING_TASK_REF
	                + "\" for the default root-node in\n"
+"                functions.  (E.g.: Revert to prior behavior, before being\n"
+"                replaced by \"" + DataComponent.CXX_TCM_ROOT_NODE + "\".)\n"
+"\n"
+"  -A <text>  -- Add text to beginning of the outputfile.  Useful for \n"
+"                inserting #includes at the beginning of the code file.\n"
+"                Note: Text must be a single string.\n"
+"                      You may need \"\"'s to shell-escape it.\n"
+"                Note: For \"-S\", text is only inserted in the header file."
                                                                         + "\n"
+"\n"
+"  -o <file>  -- Specify destination file.\n"
+"\n"
+"  <file>     -- Specify source file.  Specify no files for stdin.\n";



	/* Class Methods */
  public static void main ( String args[] )
  {
    new TDLC ( args ) . run();
  }

  public static String limitNumberOfLines ( String theString,
					    int    theNumberOfLines )
  {
    int i, index;

    if ( theString == null )
      return theString;

    for ( i=0, index=0
	    ;
	  (i < theNumberOfLines) && (index < theString.length())
	    ;
	  i++
	 )
    {
      index = theString . indexOf ( '\n', index );
      if ( index == -1 )
	return theString;
      index ++;
    }

    if ( index >= theString.length() )
      return theString;
    else
      return theString.substring ( 0, index );
  }


	/* Instance Variables */
  protected String[]  args;


	/* Instance Methods */
  public TDLC ( String[] theArgs )
  {
    args = theArgs;
  }

  public String[] getArgs() { return args; }


  public String checkThatFilenameEndsWith ( String theFilename,
					    String theSuffix    )
  {
    if ( theFilename . endsWith ( theSuffix ) )
      return theFilename;
    else
      return theFilename + theSuffix;
  }


  public File checkValidFileToWrite ( String  theFilename,
				      boolean theAutoAnswerYes )
  {
    int   inchar  = -1;
    File  tmpFile = new File ( theFilename );

		/* IF this file already exists.... */
    if ( tmpFile . exists() == true )
    {
      System.err.print ( "TDLC Warning:  Overwriting Output file \""
			 + theFilename + "\".  Continue (y/n)?   ");

      if ( theAutoAnswerYes )
      {
	System.err.println ( "YES [-y]" );
      }
      else
      {
	System.err.println ( "" );
	do
	{
	  try
	  {
	    inchar = System.in.read();
	  }
	  catch ( java.io.IOException  theException )
	  {
	    System.err.println (
			"TDLC Error:  Unable to read \"(y/n)?\" from stdin.\n"
		      + "Aborting...\nJava.io.IOException = " + theException );
	    System.exit ( -2 );
	  }

	  if ( inchar == -1 )
	  {
	    System.err.println (
		      "TDLC Error:  Unable to read \"(y/n)?\" from stdin.\n"
		      + "Aborting...\n"
		      + "No java.io.IOException, but End-Of-File reached." );
	    System.exit ( -2 );
	  }

	  if ( (inchar == 'n') || (inchar == 'N') )
	    System.exit ( -1 );

	} while ( (inchar != 'y') && (inchar != 'Y') );

      } /* if ( autoAnswerYes ) .... else .... */

    } /* if ( tmpFile . exists() == true ) */

	/* Otherwise, we can use this output-file */
    return tmpFile;
  }


  public void run()
  {
    DataVector usingQuoteDirs        = new DataVector(),
               usingAngleBracketDirs = new DataVector();

    int        i, argIndex;

    String     string    = null,
               textToAdd = "";

    String     primaryOutputFilename = null;

    File       inputFile           = null,
               outputFile          = null,
	       secondaryOutputFile = null,
               tmpFile;

    char       translateUsingStatements     = TDLC.UNSET_VALUE,
               theShowClassStructNamespaces = TDLC.UNSET_VALUE,
               forceExternTasks             = TDLC.UNSET_VALUE,
               forceNonTaskCode             = TDLC.UNSET_VALUE,
               produceFile                  = TDLC.ARG_INLINED_CXX,
	       produceType                  = TDLC.ARG_FULL_INLINE_TYPE,
               produceDistributedExceps     = TDLC.ARG_FULL_INLINE_TYPE;

    boolean    debugEnabled          = false,
               verboseRun            = false,
	       lineMacrosEnabled     = true,
               issueWarnings         = true,
               needsToRun            = true,
               autoAnswerYes         = false,
               doingLoadUsing        = false,
               hasSecondaryArgument  = false;


    for ( argIndex=0;   argIndex < getArgs().length;   argIndex++ )
    {
      if ( getArgs() [ argIndex ] . charAt ( 0 ) != '-' )
      {
	tmpFile = new File ( getArgs() [ argIndex ] );

	if ( tmpFile . exists() == false )
	{
	  System.err.println ( "TDLC Error:  Input file \""
			       + getArgs() [ argIndex ]
			       + "\" does not exist.\nUse -h for HELP." );
	  System.exit ( -2 );
	}

	inputFile = tmpFile;

	runForFile ( inputFile, outputFile, secondaryOutputFile,
		     primaryOutputFilename,
		     textToAdd, translateUsingStatements,
		     forceExternTasks, forceNonTaskCode,
		     produceFile, produceType, produceDistributedExceps,
		     doingLoadUsing, usingQuoteDirs, usingAngleBracketDirs,
		     debugEnabled, autoAnswerYes, verboseRun,
		     issueWarnings, lineMacrosEnabled,
		     theShowClassStructNamespaces );

	needsToRun            = false;
	doingLoadUsing        = false;
	inputFile             = null;
	outputFile            = null;
	secondaryOutputFile   = null;
	primaryOutputFilename = null;
      }
      else /*  == "-" ) */
      {
	needsToRun = true;

	for ( i=1;   i < getArgs() [ argIndex ] . length();   i++ )
	{
		/* We don't have a secondary argument to begin with... */
	  hasSecondaryArgument = false;

	  switch ( getArgs() [ argIndex ] . charAt ( i ) )
	  {
	    case TDLC.ARG_HELP:
	      System.out.println ( TDLC.USAGE_STRING );
	      System . exit ( 0 );
	      break; /* What the heck... */


	    case TDLC.ARG_VERSION:
	      System.out.println ( "TDLC Version:  " + TDLC.VERSION_NUMBER );
	      System . exit ( 0 );
	      break; /* What the heck... */


	    case TDLC.ARG_DEBUG:
	      debugEnabled = true;
	      break;

	    case TDLC.ARG_VERBOSE:
	      verboseRun = true;
	      break;

	    case TDLC.ARG_YES:
	      autoAnswerYes = true;
	      break;


	    case TDLC.ARG_DISABLE_LINE_MACROS:
	      lineMacrosEnabled = false;
	      break;


	    case TDLC.ARG_USE_LINE_NOT_FILENAME:
	      DataFile.setOverrideInMessagesFilenamesWithLine ( true );
	      break;


	    case TDLC.ARG_ENABLE_VALIDATION:
	      issueWarnings = true;
	      break;

	    case TDLC.ARG_DISABLE_VALIDATION:
	      issueWarnings = false;
	      break;


	    case TDLC.ARG_INCLUDE_CLASSES:
	      theShowClassStructNamespaces = TDLC.ARG_INCLUDE_CLASSES;
	      break;

	    case TDLC.ARG_REMOVE_CLASSES:
	      theShowClassStructNamespaces = TDLC.ARG_REMOVE_CLASSES;
	      break;


	    case TDLC.ARG_STANDARD_USING:
	      translateUsingStatements = TDLC.ARG_STANDARD_USING;
	      break;

	    case TDLC.ARG_NO_USING:
	      translateUsingStatements = TDLC.ARG_NO_USING;
	      break;

	    case TDLC.ARG_TRANLATE_USING:
	      translateUsingStatements = TDLC.ARG_TRANLATE_USING;
	      break;


	    case TDLC.ARG_DELETE_EXTERN_TASKS:
	      forceExternTasks = TDLC.ARG_DELETE_EXTERN_TASKS;
	      break;

	    case TDLC.ARG_ENABLE_EXTERN_TASKS:
	      forceExternTasks = TDLC.ARG_ENABLE_EXTERN_TASKS;
	      break;

	    case TDLC.ARG_DELETE_NON_TASK_CODE:
	      forceNonTaskCode = TDLC.ARG_DELETE_NON_TASK_CODE;
	      break;

	    case TDLC.ARG_ENABLE_NON_TASK_CODE:
	      forceNonTaskCode = TDLC.ARG_ENABLE_NON_TASK_CODE;
	      break;


	    case TDLC.ARG_FULL_INLINE_TYPE:
	      produceType = TDLC.ARG_FULL_INLINE_TYPE;
	      break;

	    case TDLC.ARG_FULL_EXTERN_TYPE:
	      produceType = TDLC.ARG_FULL_EXTERN_TYPE;
	      break;

	    case TDLC.ARG_HIDE_EXTERN_TYPE:
	      produceType = TDLC.ARG_HIDE_EXTERN_TYPE;
	      break;

	    case TDLC.ARG_COMBINED_EXTERN_TYPE:
	      produceType = TDLC.ARG_COMBINED_EXTERN_TYPE;
	      break;

	    case TDLC.ARG_DISTRIBUTED_ONLY_INLINE:
	      produceType = TDLC.ARG_DISTRIBUTED_ONLY_INLINE;
	      break;

	    case TDLC.ARG_DISTRIBUTED_ONLY_EXTERN:
	      produceType = TDLC.ARG_DISTRIBUTED_ONLY_EXTERN;
	      break;

	    case TDLC.ARG_DISTRIBUTED_EXCEPS_ONLY:
	      produceDistributedExceps = TDLC.ARG_DISTRIBUTED_EXCEPS_ONLY;
	      break;

	    case TDLC.ARG_DISTRIBUTED_EXCEPS_NONE:
	      produceDistributedExceps = TDLC.ARG_DISTRIBUTED_EXCEPS_NONE;
	      break;

	    case TDLC.ARG_HTML:
	      if (   ( produceFile == TDLC.ARG_STANDARD_CXX )
		  && ( outputFile != null ) )
	      {
		System.err.println (
		    "TDLC Error:  Encountered \"-" + TDLC.ARG_HTML
		    + "\" after both \"-" + TDLC.ARG_STANDARD_CXX
		    + "\" and \"-" + TDLC.ARG_OUT_FILE
		    + "\".  Aborting Compilation.  Use \"-"
		    + TDLC.ARG_HELP + "\" for help." );
		System.exit ( -1 );
	      }
	      produceFile = TDLC.ARG_HTML;
	      break;

	    case TDLC.ARG_SMART_ANALYZER:
	      if (   ( produceFile == TDLC.ARG_STANDARD_CXX )
		  && ( outputFile != null ) )
	      {
		System.err.println (
		    "TDLC Error:  Encountered \"-" + TDLC.ARG_SMART_ANALYZER
		    + "\" after both \"-" + TDLC.ARG_STANDARD_CXX
		    + "\" and \"-" + TDLC.ARG_OUT_FILE
		    + "\".  Aborting Compilation.  Use \"-"
		    + TDLC.ARG_HELP + "\" for help." );
		System.exit ( -1 );
	      }
	      produceFile = TDLC.ARG_SMART_ANALYZER;
	      doingLoadUsing = true;
	      break;

	    case TDLC.ARG_INLINED_CXX:
	      if (   ( produceFile == TDLC.ARG_STANDARD_CXX )
		  && ( outputFile != null ) )
	      {
		System.err.println (
		    "TDLC Error:  Encountered \"-" + TDLC.ARG_INLINED_CXX
		    + "\" after both \"-" + TDLC.ARG_STANDARD_CXX
		    + "\" and \"-" + TDLC.ARG_OUT_FILE
		    + "\".  Aborting Compilation.  Use \"-"
		    + TDLC.ARG_HELP + "\" for help." );
		System.exit ( -1 );
	      }
	      produceFile = TDLC.ARG_INLINED_CXX;
	      break;

	    case TDLC.ARG_HEADER_CXX:
	      if (   ( produceFile == TDLC.ARG_STANDARD_CXX )
		  && ( outputFile != null ) )
	      {
		System.err.println (
		    "TDLC Error:  Encountered \"-" + TDLC.ARG_HEADER_CXX
		    + "\" after both \"-" + TDLC.ARG_STANDARD_CXX
		    + "\" and \"-" + TDLC.ARG_OUT_FILE
		    + "\".  Aborting Compilation.  Use \"-"
		    + TDLC.ARG_HELP + "\" for help." );
		System.exit ( -1 );
	      }
	      produceFile = TDLC.ARG_HEADER_CXX;
	      break;

	    case TDLC.ARG_CODE_CXX:
	      if (   ( produceFile == TDLC.ARG_STANDARD_CXX )
		  && ( outputFile != null ) )
	      {
		System.err.println (
		    "TDLC Error:  Encountered \"-" + TDLC.ARG_CODE_CXX
		    + "\" after both \"-" + TDLC.ARG_STANDARD_CXX
		    + "\" and \"-" + TDLC.ARG_OUT_FILE
		    + "\".  Aborting Compilation.  Use \"-"
		    + TDLC.ARG_HELP + "\" for help." );
		System.exit ( -1 );
	      }
	      produceFile = TDLC.ARG_CODE_CXX;
	      break;

	    case TDLC.ARG_STANDARD_CXX:
	      if ( outputFile != null )
	      {
		System.err.println ( "TDLC Error:  \"-" + TDLC.ARG_STANDARD_CXX
				    + "\" MUST proceed \"-" + TDLC.ARG_OUT_FILE
				    + "\".  Aborting Compilation.  Use \"-"
				    + TDLC.ARG_HELP + "\" for help." );
		System.exit ( -1 );
	      }
	      produceFile = TDLC.ARG_STANDARD_CXX;
	      break;


	    case TDLC.ARG_LOAD_USING:
	      doingLoadUsing = true;
	      break;


	    case TDLC.ARG_INCLUDE_DIR:
	      hasSecondaryArgument = true;
	      break;

	    case TDLC.ARG_USING_DIR:
	      hasSecondaryArgument = true;
	      break;


	    case TDLC.ARG_USE_OLD_ROOT_NODE_VALUE:
	      DataComponent
		. restorePreviousRootNodeValueForBackwardCompatibility();
	      break;


	    case TDLC.ARG_ADD_TEXT:
	      hasSecondaryArgument = true;
	      break;

	      
	    case TDLC.ARG_OUT_FILE:
	      hasSecondaryArgument = true;
	      break;


	    default:
	      System.err.println ( "TDLC Error:  Illegal command '-"
				   + getArgs() [ argIndex ] . charAt ( i ) 
				   + "'.\nUse -h for HELP." );
	      System.exit ( -1 );
	      break;
	  } /* switch ( getArgs() [ argIndex ] . charAt ( i ) ) */


		/* Are we reading a secondary argument? */
	  if ( hasSecondaryArgument )
	  {
		/* Is it part of this string? */
	    if ( (i+1) < getArgs() [ argIndex ] . length() )
	    {
	      string = getArgs() [ argIndex ] . substring (i+1);
	    }
	    else /* Assume it's the next string... */
	    {
		/* If there is no next-string */
	      if ( getArgs() . length <= argIndex )
	      {
		System.err.println ( "TDLC Error:  Illegal command '-"
				     + TDLC.ARG_OUT_FILE
				     + "'.\nUse -h for HELP." );
		System.exit ( -1 );
	      }

	      string = getArgs() [ argIndex + 1 ];
	    }
	  }

		/* Are we adding a "#using" search directory? */
	  if ( getArgs() [ argIndex ] . charAt ( i ) == TDLC.ARG_INCLUDE_DIR )
	  {
	    tmpFile = new File ( string );

	    if ( tmpFile . isDirectory() )
	    {
	      usingQuoteDirs        . addElement ( tmpFile );
	      usingAngleBracketDirs . addElement ( tmpFile );
	    }
	    else
	    {
	      System.err.println (
		  "TDLC Warning:  Unable to search directory \"" + string
		  + "\".   Directory is not accessible." );
	    }
	  }

		/* Are we adding a "#using <>" search directory? */
	  if ( getArgs() [ argIndex ] . charAt ( i ) == TDLC.ARG_USING_DIR )
	  {
	    tmpFile = new File ( string );

	    if ( tmpFile . isDirectory() )
	    {
	      usingAngleBracketDirs . addElement ( tmpFile );
	    }
	    else
	    {
	      System.err.println (
		  "TDLC Warning:  Unable to search directory \"" + string
		  + "\".   Directory is not accessible." );
	    }
	  }


		/* Are we adding text to the output file? */ 
	  if ( getArgs() [ argIndex ] . charAt ( i ) == TDLC.ARG_ADD_TEXT )
	  {
	    textToAdd = string;
	  }


		/* Are we establishing an output file? */
	  if ( getArgs() [ argIndex ] . charAt ( i ) == TDLC.ARG_OUT_FILE )
	  {
	    if ( produceFile != TDLC.ARG_STANDARD_CXX )
	    {
	      outputFile = checkValidFileToWrite ( string, autoAnswerYes );
	    }
	    else
	    {
	      primaryOutputFilename
		= checkThatFilenameEndsWith (
			      DataFile.translateTDLFilenameToCxx ( string,
					   DataFile.STANDARD_CXX_HEADER_TAIL ),
			      DataFile.STANDARD_CXX_HEADER_TAIL );


	      outputFile = checkValidFileToWrite ( primaryOutputFilename,
						   autoAnswerYes );

	      secondaryOutputFile
		= checkValidFileToWrite ( 
			  checkThatFilenameEndsWith (
			      DataFile.translateTDLFilenameToCxx ( string,
					   DataFile.STANDARD_CXX_CODE_TAIL ),
			      DataFile.STANDARD_CXX_CODE_TAIL ),
			  autoAnswerYes );
	    }
	  }

		/* Did we read a secondary argument? */
	  if ( hasSecondaryArgument )
	  {
		/* Did we snarf the next string? */
	    if ( (i+1) >= getArgs() [ argIndex ] . length() )
	      argIndex ++;

	 	/* And there are definitely no more arguments in this string.*/
	    break; /* Terminate this FOR loop. */
	  }

	} /* for ( i=1;   i < getArgs() [ argIndex ] . length();   i++ ) */
      } /* if ( getArgs() [ argIndex ] . charAt ( 0 ) != "-" )  ... else ... */
    } /* for ( argIndex=0;   argIndex < getArgs().length;   argIndex++ ) */


	/* Do we need to run again? */
    if ( needsToRun )
    {
      runForFile ( null, outputFile, secondaryOutputFile,
		   primaryOutputFilename,
		   textToAdd, translateUsingStatements,
		   forceExternTasks, forceNonTaskCode,
		   produceFile, produceType, produceDistributedExceps,
		   doingLoadUsing, usingQuoteDirs, usingAngleBracketDirs,
		   debugEnabled, autoAnswerYes, verboseRun,
		   issueWarnings, lineMacrosEnabled,
		   theShowClassStructNamespaces );
    }

  } /* run() */


  public void runForFile ( File       theInputFile,
			   File       theOutputFile,
			   File       theSecondaryOutputFile,
			   String     thePrimaryOutputFilename,
			   String     theTextToAdd,
			   char       theTranslateUsingStatements,
			   char       theForceExternTasks,
			   char       theForceNonTaskCode,
			   char       theProduceFile,
			   char       theProduceType,
			   char       theProduceDistributedExceps,
			   boolean    theDoingLoadUsing,
			   DataVector theUsingQuoteDirs,
			   DataVector theUsingAngleBracketDirs,
			   boolean    theDoDebugOnly,
			   boolean    theAutoAnswerYes,
			   boolean    theIsVerbose,
			   boolean    theIssueWarnings,
			   boolean    theLineMacrosEnabled,
			   char       theShowClassStructNamespaces )
  {
    StringBuffer         inputData = new StringBuffer ( 1000 );
    int                  inchar;
    DataFile             dataFile = null;
    int                  cxxFirstSubsetToGenerate,
			 cxxSecondSubsetToGenerate = TDLC.NO_SUBSET;
    boolean              hasExternTasks  = true,
                         cxxFirstSubsetHasNonTaskCode  = false,
			 cxxSecondSubsetHasNonTaskCode = true;
    char                 firstTranslateUsingStatements  = TDLC.ARG_NO_USING,
                         secondTranslateUsingStatements = TDLC.ARG_NO_USING;
    boolean              firstShowClassStructNamespaces  = true,
                         secondShowClassStructNamespaces = false;
    int                  cxxDistributedExceptions = TDLC.NO_SUBSET;

    if ( theProduceDistributedExceps == TDLC.ARG_DISTRIBUTED_EXCEPS_ONLY )
      cxxDistributedExceptions = DataComponent.CXX_DISTRIBUTED_EXCEPTIONS_ONLY;
    else if ( theProduceDistributedExceps == TDLC.ARG_DISTRIBUTED_EXCEPS_NONE )
      cxxDistributedExceptions = DataComponent.CXX_DISTRIBUTED_EXCEPTIONS_NONE;

	/* By default, put everything in with the code. */
    if (   ( theProduceFile == TDLC.ARG_CODE_CXX    )
	|| ( theProduceFile == TDLC.ARG_INLINED_CXX ) )
    {
      cxxFirstSubsetHasNonTaskCode  = true;
    }


	/* Lets figure out what we are generating... */
    switch ( theProduceType )
    {
	/* Note:  First case is default case, it appears at the bottom. */

      case TDLC.ARG_FULL_EXTERN_TYPE: /*-2*/
	switch ( theProduceFile )
	{
	  case TDLC.ARG_HTML:
	    cxxFirstSubsetToGenerate = DataComponent.HTML_DOCUMENTATION;
	    break;

	  case TDLC.ARG_SMART_ANALYZER:
	    cxxFirstSubsetToGenerate = TDLC.NO_SUBSET;
	    break;

	  case TDLC.ARG_HEADER_CXX:
	    cxxFirstSubsetToGenerate = DataComponent.CXX_HEADER;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    break;

	  case TDLC.ARG_CODE_CXX:
	    cxxFirstSubsetToGenerate = DataComponent.CXX_CODE;
	    firstShowClassStructNamespaces = false;
	    break;

	  case TDLC.ARG_STANDARD_CXX:
	    cxxFirstSubsetToGenerate  = DataComponent.CXX_HEADER;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxSecondSubsetToGenerate = DataComponent.CXX_CODE;
	    break;

	  default:
	    if ( theProduceFile != TDLC.ARG_INLINED_CXX )
	      System.err.println ( "[TDLC:runForFile-1]  Warning:  "
		        + "theProduceFile is unknown. (\"-" + theProduceFile
			+ "\")  Assuming \"-" + TDLC.ARG_INLINED_CXX + "\"." );

	    cxxFirstSubsetToGenerate = DataComponent.INLINED_CXX_CODE;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    break;
	}
	break;


      case TDLC.ARG_HIDE_EXTERN_TYPE: /*-3*/
	switch ( theProduceFile )
	{
	  case TDLC.ARG_HTML:
	    cxxFirstSubsetToGenerate = DataComponent.HTML_DOCUMENTATION;
	    break;

	  case TDLC.ARG_SMART_ANALYZER:
	    cxxFirstSubsetToGenerate = TDLC.NO_SUBSET;
	    break;

	  case TDLC.ARG_HEADER_CXX:
	    cxxFirstSubsetToGenerate = DataComponent.CXX_BARE_HEADER;
	    firstShowClassStructNamespaces = false;
	    break;

	  case TDLC.ARG_CODE_CXX:
	    cxxFirstSubsetToGenerate = DataComponent.CXX_CODE_AND_HEADER;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    firstShowClassStructNamespaces = true;
	    break;

	  case TDLC.ARG_STANDARD_CXX:
	    cxxFirstSubsetToGenerate  = DataComponent.CXX_BARE_HEADER;
	    cxxSecondSubsetToGenerate = DataComponent.CXX_CODE_AND_HEADER;
	    secondTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    firstShowClassStructNamespaces = false;
	    secondShowClassStructNamespaces = true;
	    break;

	  default:
	    if ( theProduceFile != TDLC.ARG_INLINED_CXX )
	      System.err.println ( "[TDLC:runForFile-2]  Warning:  "
		        + "theProduceFile is unknown. (\"-" + theProduceFile
			+ "\")  Assuming \"-" + TDLC.ARG_INLINED_CXX + "\"." );

	    cxxFirstSubsetToGenerate = DataComponent.INLINED_CXX_CODE;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    break;
	}
	break;



      case TDLC.ARG_COMBINED_EXTERN_TYPE: /*-4*/
	switch ( theProduceFile )
	{
	  case TDLC.ARG_HTML:
	    cxxFirstSubsetToGenerate = DataComponent.HTML_DOCUMENTATION;
	    break;

	  case TDLC.ARG_SMART_ANALYZER:
	    cxxFirstSubsetToGenerate = TDLC.NO_SUBSET;
	    break;

	  case TDLC.ARG_HEADER_CXX:
	    cxxFirstSubsetToGenerate = DataComponent.CXX_BARE_HEADER;
	    firstShowClassStructNamespaces = false;
	    break;

	  case TDLC.ARG_CODE_CXX:
	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_CODE_AND_HEADER_SEQUENTIAL;
	    firstTranslateUsingStatements
	      = TDLC.ARG_TRANLATE_USING;           /*overriden below.*/
	    firstShowClassStructNamespaces = true; /*overriden below.*/
	    break;

	  case TDLC.ARG_STANDARD_CXX:
	    cxxFirstSubsetToGenerate  = DataComponent.CXX_BARE_HEADER;
	    cxxSecondSubsetToGenerate
	      = DataComponent.CXX_CODE_AND_HEADER_SEQUENTIAL;
	    secondTranslateUsingStatements
	      = TDLC.ARG_TRANLATE_USING;             /*overriden below.*/
	    firstShowClassStructNamespaces  = false; /*overriden below.*/
	    secondShowClassStructNamespaces = true;  /*overriden below.*/
	    break;

	  default:
	    if ( theProduceFile != TDLC.ARG_INLINED_CXX )
	      System.err.println ( "[TDLC:runForFile-2]  Warning:  "
		        + "theProduceFile is unknown. (\"-" + theProduceFile
			+ "\")  Assuming \"-" + TDLC.ARG_INLINED_CXX + "\"." );

	    cxxFirstSubsetToGenerate = DataComponent.INLINED_CXX_CODE;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    break;
	}
	break;



      case TDLC.ARG_DISTRIBUTED_ONLY_INLINE: /*-5*/
	switch ( theProduceFile )
	{
	  case TDLC.ARG_HTML:
	    cxxFirstSubsetToGenerate = DataComponent.HTML_DOCUMENTATION;
	    break;

	  case TDLC.ARG_SMART_ANALYZER:
	    cxxFirstSubsetToGenerate = TDLC.NO_SUBSET;
	    break;

	  case TDLC.ARG_HEADER_CXX:
	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces = false;
	    break;

	  case TDLC.ARG_CODE_CXX:
	    cxxFirstSubsetToGenerate = TDLC.GENERATE_HEADER_ONLY;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces = false;
	    break;

	  case TDLC.ARG_STANDARD_CXX:
	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxSecondSubsetToGenerate = TDLC.GENERATE_HEADER_ONLY;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces  = false;
	    secondShowClassStructNamespaces = false;
	    break;

	  default:
	    if ( theProduceFile != TDLC.ARG_INLINED_CXX )
	      System.err.println ( "[TDLC:runForFile-2]  Warning:  "
		        + "theProduceFile is unknown. (\"-" + theProduceFile
			+ "\")  Assuming \"-" + TDLC.ARG_INLINED_CXX + "\"." );

	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces  = false;
	    secondShowClassStructNamespaces = false;
	    break;
	}
	break;



      case TDLC.ARG_DISTRIBUTED_ONLY_EXTERN: /*-6*/
	switch ( theProduceFile )
	{
	  case TDLC.ARG_HTML:
	    cxxFirstSubsetToGenerate = DataComponent.HTML_DOCUMENTATION;
	    break;

	  case TDLC.ARG_SMART_ANALYZER:
	    cxxFirstSubsetToGenerate = TDLC.NO_SUBSET;
	    break;

	  case TDLC.ARG_HEADER_CXX:
	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_DISTRIBUTED_ONLY;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces  = false;
	    break;

	  case TDLC.ARG_CODE_CXX:
	    cxxFirstSubsetToGenerate = DataComponent.CXX_CODE_DISTRIBUTED_ONLY;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces  = false;
	    break;

	  case TDLC.ARG_STANDARD_CXX:
	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_DISTRIBUTED_ONLY;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxSecondSubsetToGenerate
	      = DataComponent.CXX_CODE_DISTRIBUTED_ONLY;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces  = false;
	    secondShowClassStructNamespaces = false;
	    break;

	  default:
	    if ( theProduceFile != TDLC.ARG_INLINED_CXX )
	      System.err.println ( "[TDLC:runForFile-1]  Warning:  "
		        + "theProduceFile is unknown. (\"-" + theProduceFile
			+ "\")  Assuming \"-" + TDLC.ARG_INLINED_CXX + "\"." );

	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxFirstSubsetHasNonTaskCode  = false;
	    cxxSecondSubsetHasNonTaskCode = false;
	    firstShowClassStructNamespaces  = false;
	    secondShowClassStructNamespaces = false;
	    break;
	}
	break;





      default: /*-1*/
	if ( theProduceType != TDLC.ARG_FULL_INLINE_TYPE )
	  System.err.println ( "[TDLC:runForFile-3]  Warning:  "
		   + "theProduceType is unknown. (\"-" + theProduceType
		   + "\")  Assuming \"-" + TDLC.ARG_FULL_INLINE_TYPE + "\"." );

	switch ( theProduceFile )
	{
	  case TDLC.ARG_HTML:
	    cxxFirstSubsetToGenerate = DataComponent.HTML_DOCUMENTATION;
	    break;

	  case TDLC.ARG_SMART_ANALYZER:
	    cxxFirstSubsetToGenerate = TDLC.NO_SUBSET;
	    break;

	  case TDLC.ARG_HEADER_CXX:
	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_INLINED_FUNCTIONS;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    break;

	  case TDLC.ARG_CODE_CXX:
	    cxxFirstSubsetToGenerate = DataComponent.CXX_CODE_NO_FUNCTIONS;
	    break;

	  case TDLC.ARG_STANDARD_CXX:
	    cxxFirstSubsetToGenerate
	      = DataComponent.CXX_HEADER_INLINED_FUNCTIONS;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    cxxSecondSubsetToGenerate = DataComponent.CXX_CODE_NO_FUNCTIONS;
	    break;

	  default:
	    if ( theProduceFile != TDLC.ARG_INLINED_CXX )
	      System.err.println ( "[TDLC:runForFile-0]  Warning:  "
		        + "theProduceFile is unknown. (\"-" + theProduceFile
			+ "\")  Assuming \"-" + TDLC.ARG_INLINED_CXX + "\"." );

	    cxxFirstSubsetToGenerate = DataComponent.INLINED_CXX_CODE;
	    firstTranslateUsingStatements = TDLC.ARG_TRANLATE_USING;
	    break;
	}
	break;
    }

	/* Our our defaults for Extern-Tasks/Non-Task-Code being overriden? */

    if ( theForceExternTasks == TDLC.ARG_DELETE_EXTERN_TASKS )
    {
      hasExternTasks = false;
    }

    if ( theForceExternTasks == TDLC.ARG_ENABLE_EXTERN_TASKS )
    {
      hasExternTasks = true;
    }

    if ( theForceNonTaskCode == TDLC.ARG_DELETE_NON_TASK_CODE )
    {
      cxxFirstSubsetHasNonTaskCode  = false;
      cxxSecondSubsetHasNonTaskCode = false;
    }

    if ( theForceNonTaskCode == TDLC.ARG_ENABLE_NON_TASK_CODE )
    {
      cxxFirstSubsetHasNonTaskCode  = true;
      cxxSecondSubsetHasNonTaskCode = true;
    }



	/* Are our defaults for including/removing classes/structs/namespaces
	 * being overridden?
	 */
    if ( theShowClassStructNamespaces != TDLC.UNSET_VALUE )
    {
      firstShowClassStructNamespaces
	= (theShowClassStructNamespaces == ARG_INCLUDE_CLASSES);
      secondShowClassStructNamespaces = firstShowClassStructNamespaces;
    }


	/* Our our defaults for translate-using-statements being overridden? */
    if ( theTranslateUsingStatements != TDLC.UNSET_VALUE )
    {
      firstTranslateUsingStatements  = theTranslateUsingStatements;
      secondTranslateUsingStatements = theTranslateUsingStatements;
    }


	/* Shall we print out what we are doing??? */
    if ( theDoDebugOnly || theIsVerbose )
    {
      System.err.print ( "Running with "
       + DataComponent.getCxxSubsetName ( cxxFirstSubsetToGenerate )
       + "( "
       + ( hasExternTasks ? "_HAS_ " : "NO " ) + "Extern Tasks, "
       + ( cxxFirstSubsetHasNonTaskCode ? "_HAS_ ":"NO ") + "Non-Task Code, "
       + ( cxxDistributedExceptions == TDLC.NO_SUBSET ? ""
	   : "generating " + ( ( cxxDistributedExceptions == 
				DataComponent.CXX_DISTRIBUTED_EXCEPTIONS_ONLY )
			      ? "_ONLY_" : "_NO_") + " distributed exceptions, " )
       + ( firstShowClassStructNamespaces ? "INCLUDING":"REMOVING")
       + " class/struct/namespace code. ) "
      );

      if ( cxxSecondSubsetToGenerate != TDLC.NO_SUBSET )
      {
	System.err.print ( "\n         and "
	 + DataComponent.getCxxSubsetName ( cxxSecondSubsetToGenerate )
	 + "( "
	 + ( hasExternTasks ? "_HAS_ " : "NO " ) + "Extern Tasks, "
         + ( cxxSecondSubsetHasNonTaskCode ? "_HAS_ ":"NO ") + "Non-Task Code, "
	 + ( cxxDistributedExceptions == TDLC.NO_SUBSET ? ""
	     : "generating " + ((cxxDistributedExceptions == 
				DataComponent.CXX_DISTRIBUTED_EXCEPTIONS_ONLY)
				? "_ONLY_" : "_NO_") + " distributed exceptions, ")
         + ( secondShowClassStructNamespaces ? "INCLUDING":"REMOVING")
         + " class/struct/namespace code. ) "
	);
      }

      System.err.println (     ",\n inputFile  = \""

			   + (   ( theInputFile == null )
			       ? "*STDIN*"
			       : ( theInputFile  . getAbsolutePath() ) )

			   + "\",\n outputFile = \""

			   + (   ( theOutputFile == null )
			       ? "*STDOUT*"
			       : ( theOutputFile . getAbsolutePath() ) )

			   + "\",\n textToAdd  = \"" + theTextToAdd + "\",\n"
			  );


      for ( int i=0;   i < theUsingQuoteDirs . count();  i++ )
	System.err.println ( "  #using \"\" Search Directory [ "
			     + i + " ] = \""
			     + ( ((File) ( theUsingQuoteDirs
					     . elementAt ( i ) ) )
			         . getAbsolutePath() )
			     + "\"" );
      for ( int i=0;   i < theUsingAngleBracketDirs . count();  i++ )
	System.err.println ( "  #using <> Search Directory [ "
			     + i + " ] = \""
			     + ( ((File) ( theUsingAngleBracketDirs
					     . elementAt ( i ) ) )
			         . getAbsolutePath() )
			     + "\"" );
    }

	/* If we are not doing anything else... */
    if ( theDoDebugOnly )
      return;



    if ( theIsVerbose )
      System.err.println ( "Starting..." );


	/* Shall we read our TDL-file-data in from stdin??? */
    if ( theInputFile == null )
    {
      try
      {
	while ( ( inchar = System.in.read() ) != -1 )
	  inputData . append ( (char) ( inchar & 0x00ff ) );
      }
      catch ( IOException theException )
      {
	System.err.println ( "FAILURE:  Error Reading from stdin.  ("
			     + theException . toString() + ")" );
	System.exit ( -2 );
      }
    }


    if ( theIsVerbose )
      System.err.println ( "Parsing..." );


	/* Parse that puppy in... */
    try
    {
      if ( theInputFile == null )
	dataFile = DataFile . loadDataFile ( inputData . toString() );
      else
	dataFile = DataFile . loadDataFile ( theInputFile );
    }
    catch ( DetailedParseException theException )
    {
      System.err.println ( "PARSE FAILURE:\n\n"
		 + TDLC.limitNumberOfLines ( theException . toString(), 15 ) );
      System.exit ( -3 );
    }


    if ( theIsVerbose )
      System.err.println ( "Primary Parse Ended." );



	/* Are we doing warnings? */
    if ( theIssueWarnings )
    {
      DataDestinationPrintStream    dataDestinationPrintStream
	       = new DataDestinationPrintStream ( System.err );

      DataValidateCodeReturnValue   validateCodeReturnValue
	       = new DataValidateCodeReturnValue( dataDestinationPrintStream );

      try
      {
	dataFile . validateExternalCode ( DataValidateCode.BASE_REFERENCE,
					  validateCodeReturnValue );
      }
      catch ( CompilationException theException )
      {
	System.err.println ( "\n\nTDL Semantic Violation Detected::\n\n"
			     + theException . getMessage() );
	System.exit ( -3 );
      }

      if ( validateCodeReturnValue . hasWarningsOrErrors() )
      {
	System.out.println ( "\n" + validateCodeReturnValue . toString()
			     + " found." );
      }

      if ( validateCodeReturnValue . hasErrors() )
      {
	System.exit ( -3 );
      }
    }



	/* Should we load any attached (#using...) files??? */
    if ( theDoingLoadUsing )
    {
      try
      {
	dataFile . loadAttachedFiles ( theUsingQuoteDirs,
				       theUsingAngleBracketDirs,
				       theIsVerbose );
      }
      catch ( CompilationException  theException )
      {
	System.err.print ( theException . getMessage() );
	System.exit ( -3 );
      }
    }



    if ( theIsVerbose )
      System.err.println ( "All Parsing Ended." );


	/* And print it out... */

	/* Set up double-files for StandardCxxFiles */
	/* (If necessary -- Ie: No output file was specified...) */
    if ( cxxSecondSubsetToGenerate != TDLC.NO_SUBSET )
    {
      if ( theOutputFile == null )
      {
	if ( theSecondaryOutputFile != null )
	{
	  System.err.println ( "[TDLC:runForFile]  Warning:  "
	    + "Output file is null.  Secondary Output file ignored. (reset).  "
	    + "Possible programmer error." );
	}

	if ( theInputFile == null )
	{
	  theSecondaryOutputFile = null; /* Redundant, but for safety... */
	}
	else
	{
	  theOutputFile
		= checkValidFileToWrite ( 
			      DataFile.translateTDLFilenameToCxx (
					   theInputFile . getAbsolutePath(),
					   DataFile.STANDARD_CXX_HEADER_TAIL ),
			      theAutoAnswerYes );

	  theSecondaryOutputFile
		= checkValidFileToWrite (
			      DataFile.translateTDLFilenameToCxx (
					   theInputFile . getAbsolutePath(),
					   DataFile.STANDARD_CXX_CODE_TAIL ),
			      theAutoAnswerYes );

	} /* if ( theInputFile == null ) ....  else .... */
      } /* if ( theOutputFile == null ) */

    } /* if ( theDoingStandardCxxFiles ) */


	/* Are we running just print-smart-analyzer-code? */
    if ( theProduceFile == TDLC.ARG_SMART_ANALYZER )
    {
      TDLtoSMART.printSMARTFile ( theInputFile, theOutputFile,
				  dataFile, theIsVerbose, theTextToAdd, this );
      return;
    }


	/* Print first (possibly only) file */
    printForFile ( theInputFile, theOutputFile, cxxFirstSubsetToGenerate,
		   dataFile, firstTranslateUsingStatements,
		   hasExternTasks, cxxFirstSubsetHasNonTaskCode,
		   theIsVerbose, theLineMacrosEnabled,
		   firstShowClassStructNamespaces, cxxDistributedExceptions,
		   theTextToAdd );


	/* Print second file */
    if ( cxxSecondSubsetToGenerate != TDLC.NO_SUBSET )
    {
      printForFile ( theInputFile,
		     theSecondaryOutputFile, cxxSecondSubsetToGenerate,
		     dataFile, secondTranslateUsingStatements,
		     hasExternTasks, cxxSecondSubsetHasNonTaskCode,
		     theIsVerbose, theLineMacrosEnabled,
		     secondShowClassStructNamespaces, cxxDistributedExceptions,
		     (  ( theOutputFile == null )
		       ? ""
		       : ( "#include <"
			   + (   ( thePrimaryOutputFilename != null )
			       ? thePrimaryOutputFilename
			       : theOutputFile . getName()
			      )
			   + ">"
			  )
		      ) );
    }
  }




  public void printForFile ( File     theInputFile,
			     File     theOutputFile,
			     int      theCxxSubsetToGenerate,
			     DataFile theDataFile,
			     char     theTranslateUsingStatements,
			     boolean  theHasExternTasks,
			     boolean  theHasNonTaskCode,
			     boolean  theIsVerbose,
			     boolean  theLineMacrosEnabled,
			     boolean  theShowClassStructNamespaces,
			     int      theCxxDistributedExceptions,
			     String   theStartFileString )
  {
    DataDestinationFile  fileDestination = new DataDestinationFile();
    DataDestination      dataDestination = null;
    int                  translateUsingMethod;
    DateFormat           dateFormatter;


	/* Configure DateFormat */
    dateFormatter = DateFormat.getDateTimeInstance ( DateFormat.FULL,
						     DateFormat.LONG );
	/* For some reason DateFormat defaults to PST *
	 * instead of the local timezone.             */
    dateFormatter . setTimeZone ( TimeZone.getDefault() );



	/* Find out how we are translating our using statements */
    switch ( theTranslateUsingStatements )
    {
      case TDLC.ARG_STANDARD_USING:
	translateUsingMethod = DataFile.SHOW_USING;
	break;

      case TDLC.ARG_NO_USING:
        translateUsingMethod = DataFile.HIDE_USING;
	break;

      case TDLC.ARG_TRANLATE_USING:
	translateUsingMethod = DataFile.TRANSLATE_USING;
	break;

      default:
	System.err.println ( "[TDLC:printForFile]  Warning:  "
		     + "Unknown translate-using-statements directive (\"-"
		     + theTranslateUsingStatements + "\")." );
	translateUsingMethod = DataFile.SHOW_USING;
	break;
    }


    try
    {
	/* Where are we generating to? */
      if ( theOutputFile == null )
      {
	if ( theIsVerbose )
	  System.out.println ( "\nResults:" );

	dataDestination = new DataDestinationStringBuffer();
      }
      else
      {
		/* Is this an acceptable file? */
	if ( fileDestination . setFile ( theOutputFile . getAbsolutePath() )
	     == false )
	{
	  System.err.println ( "TDLC Error:  Failed to open output file  \""
			       + theOutputFile . getAbsolutePath() + "\"." );
	  System.exit ( -2 );
	}
	else
	{
	  dataDestination = fileDestination;
	}
      }


	/* Set our #line number filename string */
      dataDestination . setEnableLineMacros ( theLineMacrosEnabled );
      if ( theInputFile == null )
	dataDestination . setTdlFileName ( "*STDIN*" );
      else
	dataDestination . setTdlFileName ( theInputFile . getName() );
      if ( theOutputFile == null )
	dataDestination . setCxxFileName ( "*STDOUT*" );
      else
	dataDestination . setCxxFileName ( theOutputFile . getName() );


	/* Write auto-generated comment-header at the beginning of the file */
      if ( theCxxSubsetToGenerate == DataComponent.HTML_DOCUMENTATION )
      {
	dataDestination . write ( "<!--\n" );
      }
      dataDestination . write ( "/* File generated by TDLC version " );
      dataDestination . write ( TDLC.VERSION_NUMBER );
      dataDestination . write ( "\n * On.................: " );
      dataDestination . write ( dateFormatter . format ( new Date() ) );
      dataDestination . write ( "\n * Source      File...: " );
      if ( theInputFile == null )
	dataDestination . write ( "*STDIN*" );
      else
	dataDestination . write ( theInputFile . getAbsolutePath() );
      dataDestination . write ( "\n * Destination File...: " );
      if ( theOutputFile == null )
	dataDestination . write ( "*STDOUT*" );
      else
	dataDestination . write ( theOutputFile . getAbsolutePath() );
      dataDestination . write ( "\n * Command Line.......: java TDLC" );
      for ( int i=0;  i < getArgs().length; i++ )
      {
	dataDestination . write ( " " );
	dataDestination . write ( getArgs() [ i ] );
      }
      dataDestination . write ( "\n */\n" );
      if ( theCxxSubsetToGenerate == DataComponent.HTML_DOCUMENTATION )
      {
	dataDestination . write ( "-->\n" );
      }


	/* Write any beginning-of-file strings... */
      dataDestination . write ( theStartFileString );
      dataDestination . write ( "\n" );


	/* And generate that puppy */
      if (    theCxxSubsetToGenerate
	   != DataComponent.CXX_CODE_AND_HEADER_SEQUENTIAL )
      {
	    /* Sometimes, we just want the header as a placeholder... */
	if ( theCxxSubsetToGenerate != TDLC.GENERATE_HEADER_ONLY )
	{
	  theDataFile . generate ( dataDestination, theCxxSubsetToGenerate,
				   translateUsingMethod,
				   theCxxDistributedExceptions,
				   theHasExternTasks, theHasNonTaskCode,
				   theShowClassStructNamespaces );
	}
      }
      else
      {
	theDataFile . generate ( dataDestination, DataComponent.CXX_HEADER,
				 translateUsingMethod,
				 theCxxDistributedExceptions,
				 theHasExternTasks,
				 false, /* Don't include non-task code. */
				 theShowClassStructNamespaces );
	theDataFile . generate ( dataDestination, DataComponent.CXX_CODE,
				 DataFile.HIDE_USING,
				 theCxxDistributedExceptions,
				 false, /* No extern tasks */
				 theHasNonTaskCode,
				 false /* Remove class/struct/namespace*/);
      }


	/* And finish up... */
      if ( theOutputFile == null )
      {
	System.out.print ( ((DataDestinationStringBuffer) dataDestination)
			     . getString() );
      }
      else
      {
	fileDestination . close();
      }
    }
    catch ( CompilationException  theException )
    {
      System.err.println ( "" );
      System.err.println ( theException . getMessage() );
      System.err.println ( "" );
      System.exit ( -3 );
    }
  }

}
