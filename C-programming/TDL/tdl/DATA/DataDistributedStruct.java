
/*
 * This represents:
 *      DISTRIBUTED struct [ID] {  (decls;)+  } ;
 * It is also used to represent the results of parseDistributedStructInternal:
 * (For use in embedding in typedefs.)
 *      struct [ID] {  (decls;)+  }
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataDistributedStruct extends DataComponent
{
	/* Class Variables */
  public final static String   DISTRIBUTED = "DISTRIBUTED";
  public final static String   STRUCT      = "struct";
  public final static String   NAME_INDEX  = "STRUCT_NAME_INDEX";
  public final static String   OPEN_BRACE  = "{";
  public final static String   DECLARATION = "DECLARATION_";
  public final static String   CLOSE_BRACE = "}";
  public final static String   SEMICOLON   = ";";

	/* Instance Variables */
  protected String      structName;
  protected DataVector  vectorOfDataDistributedDeclarationSets;

	/* Instance Methods */
  public DataDistributedStruct()
  {
    structName = null;
    vectorOfDataDistributedDeclarationSets = new DataVector( 10 /*Arbitrary*/);
  }

  public String  getStructName() { return     structName;            }
  public boolean hasStructName() { return (getStructName() != null); }
  public void    setStructName(   String   theStructName )
			    { structName = theStructName; }

  public DataVector getVectorOfDataDistributedDeclarationSets()
	  { return vectorOfDataDistributedDeclarationSets; }

  public void addDataDistributedDeclarationSet (
	        DataDistributedDeclarationSet theDataDistributedDeclarationSet)
   { getVectorOfDataDistributedDeclarationSets()
       . addElement ( theDataDistributedDeclarationSet ); }

  public void clear()
    { getVectorOfDataDistributedDeclarationSets() . removeAllElements(); }

  public DataDistributedDeclarationSet getDataDistributedDeclarationSet (
								 int theIndex )
  {
    if (   ( theIndex <  0                                                  )
	|| ( theIndex >= getVectorOfDataDistributedDeclarationSets().size() ) )
    {
      System.err.println (
	"[DataDistributedStruct:getDataDistributedDeclarationSet]  Error:  "
	+ "Illegal index (" + theIndex + ").  Only "
	+ getVectorOfDataDistributedDeclarationSets().size()
	+ " elements in vectorOfdataDistributedDeclarationSets." );
      return null;
    }
    else
    {
      return ((DataDistributedDeclarationSet)
	      (getVectorOfDataDistributedDeclarationSets()
	         . elementAt ( theIndex ) ));
    }
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
    int i;

       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* Code/html is a no-op. */
    if (   ( isCxxCodeSubset ( theObjectSubsetToGenerate )                 )
	|| ( theObjectSubsetToGenerate == DataComponent.HTML_DOCUMENTATION ) )
      return;

    initializeGenerateSubcomponentIndex();


	/* Write out DISTRIBUTED.                               *
	 * Caveat:  When embedded into typedefs, via the method *
	 * TDLParser.parseDistributedStructInternal(...),       *
	 * we don't want to print this...                       */
    if ( hasIndex ( DataDistributedStruct.DISTRIBUTED ) )
    {
      generateSubcomponents ( DataDistributedStruct.DISTRIBUTED,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      if ( theObjectSubsetToGenerate != ENTIRE_OBJECT )
	theOutputDestination . write ( "/*" );

      theOutputDestination . write ( DataDistributedStruct.DISTRIBUTED );

      if ( theObjectSubsetToGenerate != ENTIRE_OBJECT )
	theOutputDestination . write ( "*/" );
    } /* if ( hasIndex ( DataDistributedStruct.DISTRIBUTED ) ) */



	/* Write out Struct */
    generateSubcomponents ( DataDistributedStruct.STRUCT,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    theOutputDestination . write ( DataDistributedStruct.STRUCT );



	/* Write out Struct-id, if we have one. */
    generateSubcomponents ( DataDistributedStruct.NAME_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    if ( hasStructName() )
      theOutputDestination . write ( getStructName() );



	/* Write out open-brace */
    generateSubcomponents ( DataDistributedStruct.OPEN_BRACE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    theOutputDestination . write ( DataDistributedStruct.OPEN_BRACE );



	/* Write out the DataDistributedDeclarationSet's */
    for ( i = 0;
	  i < getVectorOfDataDistributedDeclarationSets() . size();
	  i ++ )
    {
      generateSubcomponents ( DataDistributedStruct.DECLARATION + i,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      getDataDistributedDeclarationSet ( i )
	. generate ( theOutputDestination,
		     theObjectSubsetToGenerate);
    }


	/* Write out close-brace */
    generateSubcomponents ( DataDistributedStruct.CLOSE_BRACE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    theOutputDestination . write ( DataDistributedStruct.CLOSE_BRACE );


	/* Write out the semicolon.
	 * Caveat:  When embedded into typedefs, via the method *
	 * TDLParser.parseDistributedStructInternal(...),       *
	 * we don't want to print this...                       */
    if ( hasIndex ( DataDistributedStruct.SEMICOLON ) )
    {
      generateSubcomponents ( DataDistributedStruct.SEMICOLON,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      theOutputDestination . write ( DataDistributedStruct.SEMICOLON );
    } /* if ( hasIndex ( DataDistributedStruct.SEMICOLON ) ) */

	/* Deal with and leftovers. */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );



	/* If we have a name, this is in a header file,             *
	 * and we are not internal to another object via the method *
	 * TDLParser.parseDistributedStructInternal(...),           *
	 * create the FORMAT #define.                               */
    if (   ( hasStructName()                                 )
	&& ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
	&& ( hasIndex ( DataDistributedStruct.DISTRIBUTED  ) ) )
    {
      generateFormatDefineMacro ( theOutputDestination, null, null,
				  true  /* theClearMacrosSoFar */  );
    }
  }


	/* theStructName overrides getStructName(), and is used to
	 * establish aliases for objects (struct/enums) we contain.
	 */
  public void generateFormatDefineMacro (
				 DataDestination  theOutputDestination,
				 String           theQualifiedClassNameHeader,
				 String           theStructName,
				 boolean          theClearMacrosSoFar )
  {
    int i;

	/* IF theStructName != null, don't print our #define.
	 * -- It's been done elsewhere. -- Just print our children...
	 */
    if ( theStructName == null )
    {
      theStructName = getStructName();

      if ( theStructName == null )
	return;

	/* If necessary, start with a clean (cleared) list of macros    *
	 * that we are verifying the existence of via #ifndef #error... */
      if ( theClearMacrosSoFar )
	writeDistributedMacroRequirements ( theOutputDestination,
					    theClearMacrosSoFar );

      if ( theOutputDestination . getColumn() != 0 )
	theOutputDestination . write ( "\n" );      

      theOutputDestination . write ( "#define " );

      if ( theQualifiedClassNameHeader != null )
      {
	theOutputDestination . write ( theQualifiedClassNameHeader );
	theOutputDestination . write (
	  DataComponent.CXX_DISTRIBUTED_QUALIFIED_CLASSNAME_SEPARATOR );
      }

      theOutputDestination . write ( theStructName );

      theOutputDestination
	. write ( DataComponent.CXX_DISTRIBUTED_FORMAT_TRAILER );

      theOutputDestination . write ( " " );

      if ( getDistributedFormatRequiresDynamicString() )
	theOutputDestination . write ( "( " );

      theOutputDestination . setIsConcatenatingStrings ( true  );

      theOutputDestination
	. write ( getDistributedFormatString (
		    true /* Add quotes */,
		    true /* Add Dynamic String Buffer, if needed. */ ) );

      theOutputDestination . setIsConcatenatingStrings ( false );

      if ( getDistributedFormatRequiresDynamicString() )
	theOutputDestination . write ( " )" );

    } /* IF ( theStructName == null ) */


    if ( theQualifiedClassNameHeader == null )
      theQualifiedClassNameHeader = theStructName;
    else
    {
      theQualifiedClassNameHeader
	= theQualifiedClassNameHeader
	  + DataComponent.CXX_DISTRIBUTED_QUALIFIED_CLASSNAME_SEPARATOR
	  + theStructName;
    }

	/* Check for sub-contained structs/enums that need a format macro */
    for ( i=0;  i < getVectorOfDataDistributedDeclarationSets() . size();  i++)
    {
      switch ( getDataDistributedDeclarationSet ( i )
	         . getFirstDeclarationCreatingIfNecessary()
	         . getDistributedType() )
      {
	case DataTaskArgument.DATA_DISTRIBUTED_STRUCT:
	  if (    getDataDistributedDeclarationSet ( i )
		    . getFirstDeclarationCreatingIfNecessary()
		    . getDataDistributedStruct()
	       != null )
	    getDataDistributedDeclarationSet ( i )
	      . getFirstDeclarationCreatingIfNecessary()
	      . getDataDistributedStruct()
	      . generateFormatDefineMacro ( theOutputDestination,
					    theQualifiedClassNameHeader,
					    null,
					    false /* theClearMacrosSoFar */ );
	  else
	    System.err.println (
	      "[DataDistributedStruct:generateFormatDefineMacro]  Warning: "
	      + "Internal programmer error encountered:  No "
	      + "DataDistributedStruct object available for struct." );
	  break;

	case DataTaskArgument.DATA_DISTRIBUTED_ENUM:
	  if (    getDataDistributedDeclarationSet ( i )
		    . getFirstDeclarationCreatingIfNecessary()
		    . getDataDistributedEnum()
	       != null )
	    getDataDistributedDeclarationSet ( i )
	      . getFirstDeclarationCreatingIfNecessary()
	      . getDataDistributedEnum()
	      . generateFormatDefineMacro ( theOutputDestination,
					    theQualifiedClassNameHeader,
					    null,
					    false /* theClearMacrosSoFar */ );

	  else
	    System.err.println (
	      "[DataDistributedStruct:generateFormatDefineMacro]  Warning: "
	      + "Internal programmer error encountered:  No "
	      + "DataDistributedEnum object available for enum." );
      }
    } /* FOR (0 <= i < getVectorOfDataDistributedDeclarationSets().size()) */


    if ( theQualifiedClassNameHeader == null )
      theOutputDestination . write ( "\n" );
  }




  public void writeDistributedMacroRequirements (
					DataDestination  theOutputDestination,
					boolean          theClearMacrosSoFar  )
  {
    int i;

	/* If we are not embedded in another object, clear the list-so-far */
    if ( theClearMacrosSoFar )
    {
      DataTaskArgument.writeDistributedMacroRequirements( theOutputDestination,
							  null,
							  true );
    }

    for ( i=0;  i < getVectorOfDataDistributedDeclarationSets() . size();  i++)
    {
      DataTaskArgument.writeDistributedMacroRequirements (
	theOutputDestination,
	getDataDistributedDeclarationSet ( i )
	  . getTaskArgument ( 0 ) );
    } /* FOR (0 <= i < getVectorOfDataDistributedDeclarationSets().size()) */
  }

  public boolean getDistributedFormatRequiresDynamicString()
  {
    int i;

    for ( i=0;  i < getVectorOfDataDistributedDeclarationSets() . size();  i++)
    {
      if ( getDataDistributedDeclarationSet ( i )
	     . getDistributedFormatRequiresDynamicString() )
      {
	return true;
      }
    } /* FOR (0 <= i < getVectorOfDataDistributedDeclarationSets().size()) */

    return false;
  }

  public String getDistributedFormatString(boolean theAddQuotes,
					   boolean theAllowDynamicStringBuffer)
  {
    StringBuffer stringBuffer = new StringBuffer( 100 /*Arbitrary*/ );
    int          i;

	/* Deal with the dyanamic string buffer bit first -- for all entries.*/
    if ( theAllowDynamicStringBuffer )
    {
      if ( getDistributedFormatRequiresDynamicString() )
      {
	stringBuffer . append ( 
	  DataTaskArgument.getDistributedFormat_DynamicStringBufferLead() );
      }
    } /* if ( theAllowDynamicStringBuffer ) */

    if ( theAddQuotes )
      stringBuffer . append ( "\"" );      

	/* It's a struct... */
    stringBuffer . append ( "{\"" );

	/* Generate all the entries... */
    for ( i=0;  i < getVectorOfDataDistributedDeclarationSets() . size();  i++)
    {
      stringBuffer . append (

        DataTaskArgument.getDistributedFormatString ( 

	  getDataDistributedDeclarationSet ( i ) . getTaskArgumentsVector(),

	  this,

	  "Distributed Struct \""
	  + getDataDistributedDeclarationSet ( i )
	      . toString ( true ) . trim()
	  + "\"",

	  null,

	  null,

	  false /* Disable any further dynamic string buffers */ ) );

      if ( (i+1) < getVectorOfDataDistributedDeclarationSets() . size() )
	stringBuffer . append ( "\",\"" );
    }

	/* It's a struct... */
    stringBuffer . append ( "\"}" );

    if ( theAddQuotes )
      stringBuffer . append ( "\"" );      

    return stringBuffer . toString();

  } /* public String getDistributedFormatString() */

}
