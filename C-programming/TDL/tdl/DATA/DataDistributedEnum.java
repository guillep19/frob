
/*
 * This represents:
 *      DISTRIBUTED enum   [ID] {  (name[=value])+ } ;
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public class DataDistributedEnum extends DataComponent
{
	/* Class Variables */
  public final static String   DISTRIBUTED = "DISTRIBUTED";
  public final static String   ENUM        = "enum";
  public final static String   NAME_INDEX  = "ENUM_NAME_INDEX";
  public final static String   OPEN_BRACE  = "{";
  public final static String   ENUM_ENTRY  = "ENUM_ENTRY_";
  public final static String   COMMA       = ",";
  public final static String   CLOSE_BRACE = "}";
  public final static String   SEMICOLON   = ";";

	/* Instance Variables */
  protected String       enumName;
  protected DataVector  vectorOfDataDistributedEnumEntries;

	/* Instance Methods */
  public DataDistributedEnum()
  {
    enumName = null;
    vectorOfDataDistributedEnumEntries = new DataVector ( 10 /*Arbitrary*/ );
  }


  public String  getEnumName() { return     enumName;            }
  public boolean hasEnumName() { return (getEnumName() != null); }
  public void    setEnumName(   String   theEnumName )
			    { enumName = theEnumName; }

  public DataVector getVectorOfDataDistributedEnumEntries()
	  { return vectorOfDataDistributedEnumEntries; }

  public void addDataDistributedEnumEntry (
			 DataDistributedEnumEntry theDataDistributedEnumEntry )
   { getVectorOfDataDistributedEnumEntries()
       . addElement ( theDataDistributedEnumEntry ); }

  public void clear()
    { getVectorOfDataDistributedEnumEntries() . removeAllElements(); }

  public DataDistributedEnumEntry getDataDistributedEnumEntry ( int theIndex )
  {
    if (   ( theIndex <  0                                                )
	|| ( theIndex >= getVectorOfDataDistributedEnumEntries() . size() ) )
    {
      System.err.println (
	"[DataDistributedEnum:getDataDistributedEnumEntry]  Error:  "
	+ "Illegal index (" + theIndex + ").  Only "
	+ getVectorOfDataDistributedEnumEntries() . size()
	+ " elements in vectorOfDataDistributedEnumEntries." );
      return null;
    }
    else
    {
      return ((DataDistributedEnumEntry)
	      (getVectorOfDataDistributedEnumEntries()
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
	 * TDLParser.parseDistributedEnumInternal(...),         *
	 * we don't want to print this...                       */
    if ( hasIndex ( DataDistributedStruct.DISTRIBUTED ) )
    {
      generateSubcomponents ( DataDistributedEnum.DISTRIBUTED,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      if ( theObjectSubsetToGenerate != ENTIRE_OBJECT )
	theOutputDestination . write ( "/*" );

      theOutputDestination . write ( DataDistributedEnum.DISTRIBUTED );

      if ( theObjectSubsetToGenerate != ENTIRE_OBJECT )
	theOutputDestination . write ( "*/" );
    } /* if ( hasIndex ( DataDistributedStruct.DISTRIBUTED ) ) */


	/* Write out Enum */
    generateSubcomponents ( DataDistributedEnum.ENUM,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    theOutputDestination . write ( DataDistributedEnum.ENUM );



	/* Write out ENUM-id, if we have one. */
    generateSubcomponents ( DataDistributedEnum.NAME_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    if ( hasEnumName() )
      theOutputDestination . write ( getEnumName() );



	/* Write out open-brace */
    generateSubcomponents ( DataDistributedEnum.OPEN_BRACE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    theOutputDestination . write ( DataDistributedEnum.OPEN_BRACE );


	/* Write out the DataDistributedEnumEntry's */
    for ( i = 0;
	  i < getVectorOfDataDistributedEnumEntries() . size();
	  i ++ )
    {
      generateSubcomponents ( DataDistributedEnum.ENUM_ENTRY + i,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      getDataDistributedEnumEntry ( i )
	. generate ( theOutputDestination,
		     theObjectSubsetToGenerate );

      if ( (i+1) < getVectorOfDataDistributedEnumEntries() . size() )
      {
	generateSubcomponents ( DataDistributedEnum.COMMA + (i+1),
				theOutputDestination,
				theObjectSubsetToGenerate, false );

	theOutputDestination . write ( DataDistributedEnum.COMMA );
      }
    }


	/* Write out close-brace */
    generateSubcomponents ( DataDistributedEnum.CLOSE_BRACE,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

    theOutputDestination . write ( DataDistributedEnum.CLOSE_BRACE );


	/* Write out the semicolon.
	 * Caveat:  When embedded into typedefs, via the method *
	 * TDLParser.parseDistributedStructInternal(...),       *
	 * we don't want to print this...                       */
    if ( hasIndex ( DataDistributedStruct.SEMICOLON ) )
    {
      generateSubcomponents ( DataDistributedEnum.SEMICOLON,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      theOutputDestination . write ( DataDistributedEnum.SEMICOLON );
    } /* if ( hasIndex ( DataDistributedStruct.SEMICOLON ) ) */


	/* Deal with and leftovers. */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );



	/* If we have a name, this is in a header file,             *
	 * and we are not internal to another object via the method *
	 * TDLParser.parseDistributedStructInternal(...),           *
	 * create the FORMAT #define.                               */
    if (   ( hasEnumName()                                   )
	&& ( isCxxHeaderSubset ( theObjectSubsetToGenerate ) )
	&& ( hasIndex ( DataDistributedStruct.DISTRIBUTED  ) ) )
    {
      generateFormatDefineMacro ( theOutputDestination, null, null,
				  true  /* theClearMacrosSoFar */  );
    }
  }


  public void generateFormatDefineMacro (
				 DataDestination  theOutputDestination,
				 String           theQualifiedClassNameHeader,
				 String           theEnumName,
				 boolean          theClearMacrosSoFar )
  {
    if ( theEnumName == null )
      theEnumName = getEnumName();

    if ( theEnumName == null )
      return;

    if ( theOutputDestination . getColumn() != 0 )
      theOutputDestination . write ( "\n" );      

    theOutputDestination . write ( "#define " );

    if ( theQualifiedClassNameHeader != null )
    {
      theOutputDestination . write ( theQualifiedClassNameHeader );
      theOutputDestination . write (
	DataComponent.CXX_DISTRIBUTED_QUALIFIED_CLASSNAME_SEPARATOR );
    }

    theOutputDestination . write ( theEnumName );
      
    theOutputDestination
      . write ( DataComponent.CXX_DISTRIBUTED_FORMAT_TRAILER );

    theOutputDestination . write ( " " );

    theOutputDestination
      . write ( getDistributedFormatString (
		  true /* Add quotes */,
		  true /* Add Dynamic String Buffer, if needed. */ ) );

    if ( theQualifiedClassNameHeader == null )
      theOutputDestination . write ( "\n" );
  }


  public boolean getDistributedFormatRequiresDynamicString()
  {
    return false;
  }

  public String getDistributedFormatString(boolean theAddQuotes,
					   boolean theAllowDynamicStringBuffer)
  {
    StringBuffer stringBuffer = new StringBuffer( 100 /*Arbitrary*/ );
    int i;

	/* IPC can format enum's in several possible ways, depending on:
	 *   - If there are no "equal" values.
	 *   - If there are only positive "equal" values.
	 *   - If there is at least one negative "equal" values.
	 *
	 * E.g.:
         *   enum { A, B, C, D };         -> "{enum: A, B, C, D}"
	 *   enum { E=1, F=2, G=4, H=8 }; -> "{enum: 8}"
	 *   enum { X=-1, Y=-2 };         -> "int"
	 *
	 * There appears to be no significant advantage to using "{enum: 8}"
	 * over "int", so we will simplify things and use "int" for the
	 * 2nd case.  (Which greatly simplifies the complicating case in which
	 * "equal" values contain expressions that we can not evaluate at
	 * this stage of the translation process.)
	 */

    if ( theAddQuotes )
      stringBuffer . append ( "\"" );	

	/* Do we have any Enum-Entries that have an equals sign? */
    for ( i = 0;
	  i < getVectorOfDataDistributedEnumEntries() . size();
	  i ++ )
    {
      if ( getDataDistributedEnumEntry ( i ) . hasEquals() )
	break;
    }

        /* If equals-signs were found, use "int" */
    if ( i < getVectorOfDataDistributedEnumEntries() . size() )
    {
      stringBuffer . append ( "int" );
    }
    else /* No equals signs were found, use case #1 */
    {
      stringBuffer . append ( "{enum " );
      for ( i = 0;
	    i < getVectorOfDataDistributedEnumEntries() . size();
	    i ++ )
      {
	stringBuffer . append ( getDataDistributedEnumEntry ( i ) . getId() );

	if ( (i+1) < getVectorOfDataDistributedEnumEntries() . size() )
	  stringBuffer . append ( ", " );
      }
      stringBuffer . append ( "}" );
    }

    if ( theAddQuotes )
      stringBuffer . append ( "\"" );

    return stringBuffer . toString();
  }
}


