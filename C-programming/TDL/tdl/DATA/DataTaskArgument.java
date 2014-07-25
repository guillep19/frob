/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataTaskArgument extends DataComponent
{
	/* Class Constants */
  public    final static int       IN_DIRECTION      = 0;
  public    final static int       OUT_DIRECTION     = 1;
  public    final static int       INOUT_DIRECTION   = 2;
  public    final static int       NO_DIRECTION      = 3;
  public    final static int       DEFAULT_DIRECTION = NO_DIRECTION;
  public    final static int       INVALID_DIRECTION = -1;
  protected final static String[]  ARGUMENT_TYPES = { "IN", "OUT", "INOUT",""};

	/* Distributed DataTaskArgument constants */
  public    final static int       INT1                    = 0;
  public    final static int       INT2                    = 1;
  public    final static int       INT4                    = 2;
  public    final static int       INT8                    = 3;
  public    final static int       U_INT1                  = 4;
  public    final static int       U_INT2                  = 5;
  public    final static int       U_INT4                  = 6;
  public    final static int       U_INT8                  = 7;
  public    final static int       FLOAT                   = 8;
  public    final static int       DOUBLE                  = 9;
  public    final static int       BOOLEAN                 = 10;
  public    final static int       STRING                  = 11;
  public    final static int       IDENTIFIER              = 12;
  public    final static int       STRUCT                  = 13;
  public    final static int       ENUM                    = 14;
  public    final static int       DATA_DISTRIBUTED_STRUCT = 15;
  public    final static int       DATA_DISTRIBUTED_ENUM   = 16;

  public    final static int       FIRST_TYPE= INT1;
  public    final static int       LAST_TYPE = DATA_DISTRIBUTED_ENUM;

  public    final static String [] DISTRIBUTED_TYPE_STRINGS
				    = { "int1",   "int2",   "int4",     "int8",
					"u_int1", "u_int2", "u_int4", "u_int8",
					"float",  "double", "BOOLEAN","STRING",
					"IDENTIFIER", "struct", "enum",
				        "DataDistributedStruct",
				        "DataDistributedEnum"
				      };

  public    final static String [] DISTRIBUTED_ARGUMENT_FORMAT_STRINGS
				    = { "char",   "short",  "int",        null,
					"uchar",  "ushort", "uint",       null,
					"float",  "double", "Boolean","string"
				      };


	/* Constants... */
  public    final static String PERSISTENT               = "PERSISTENT";
  public    final static String ARGUMENT_DIRECTION_INDEX = "ArgDir";
  public    final static String ARGUMENT_NAME_INDEX      = "ArgName";
	/* Note:  Index *OF* the equals-sign */
  public    final static String ARGUMENT_EQUALS_INDEX    = "ArgEquals";
  public    final static String FORMAT                   = "FORMAT";
  public    final static String FORMAT_EXPRESSION        = "FormatExpression";

	/* Possible subsets of this task-argument. */
  public    final static int    TYPE_NAME_AND_EQUALS                  = 201;
  public    final static int    TYPE_AND_NAME                         = 202;
  public    final static int    AFTER_EQUALS                          = 203;
  public    final static int    TYPE_NAME_AND_COMMENTED_EQUALS        = 204;
  public    final static int    TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT = 205;


	/* Class methods */
  public static int getArgumentDirectionOfString ( String theArgumentDirection)
  {
    for ( int i=0;  i < DataTaskArgument.ARGUMENT_TYPES.length;  i++ )
    {
      if ( DataTaskArgument.ARGUMENT_TYPES [ i ]
	   . equalsIgnoreCase ( theArgumentDirection ) )
      {
	return i;
      }
    }
    return DataTaskArgument . INVALID_DIRECTION;
  }


	/************************************************
	 * These need to be written in too many places, *
	 * so lets write them just once here.           *
	 ************************************************/
  private static DataVector macrosSoFarVector
			     = new DataVector ( 100 /*arbitrary large size*/ );

  public synchronized static void writeDistributedMacroRequirements (
				      DataDestination  theOutputDestination,
				      DataVector       theTaskArgumentsVector,
				      boolean          theClearMacrosSoFar )
  {
    if ( theClearMacrosSoFar )
      DataTaskArgument.macrosSoFarVector . removeAllElements();

	/* Add each element of theTaskArgumentsVector */
    if ( theTaskArgumentsVector != null )
    {
      for ( int i=0;   i < theTaskArgumentsVector . size();   i++ )
      {
	DataTaskArgument.writeDistributedMacroRequirements (
	  theOutputDestination,
	  ((DataTaskArgument) (theTaskArgumentsVector . elementAt ( i ))),
	  DataTaskArgument.macrosSoFarVector );
      }
    }
  }

  public synchronized static void writeDistributedMacroRequirements (
				      DataDestination  theOutputDestination,
				      DataTaskArgument theTaskArgument )
  {
    DataTaskArgument.writeDistributedMacroRequirements (
      theOutputDestination,
      theTaskArgument,
      DataTaskArgument.macrosSoFarVector );
  }

  public synchronized static void writeDistributedMacroRequirements (
				      DataDestination  theOutputDestination,
				      DataTaskArgument theTaskArgument,
				      DataVector       theMacrosSoFarVector )
  {
	/* Idiocy check. */
    if ( theTaskArgument == null )
      return;

    if (   (   ( theTaskArgument.getDistributedType() == IDENTIFIER )
	    || ( theTaskArgument.getDistributedType() == STRUCT     )
	    || ( theTaskArgument.getDistributedType() == ENUM       ) )
	&& (     theTaskArgument.getDistributedId()   != null         ) )
    {
      DataTaskArgument.writeDistributedMacroRequirements (
	theOutputDestination,
	theTaskArgument.getDistributedIdTranslatingScoping(),
	theMacrosSoFarVector );
    }

    else if (
	 ( theTaskArgument.getDistributedType() == DATA_DISTRIBUTED_STRUCT )
      && ( theTaskArgument.getDataDistributedStruct() != null              ) )
    {
      theTaskArgument.getDataDistributedStruct()
	. writeDistributedMacroRequirements ( theOutputDestination,
					      false /*theClearMacrosSoFar*/ );
    }
  }

  public synchronized static void writeDistributedMacroRequirements (
				      DataDestination  theOutputDestination,
				      String           theNameString,
				      DataVector       theMacrosSoFarVector )
  {
    if ( theMacrosSoFarVector != null )
    {
      if ( theMacrosSoFarVector . contains ( theNameString ) )
	return; /* Skip if we have already done this name-string. */
      else
	theMacrosSoFarVector . addElement ( theNameString );
    }

	/* Note:  For future reference:
	 * There's a lot that can happen with DataDestination, in terms of
	 * code translation, indenting, adding leading text, etc.
	 * Deal with these features as they become necessary...
	 */
    int indent = theOutputDestination . getIndent();
    theOutputDestination . removeIndent ( indent );
    
    if ( theOutputDestination . getColumn() != 0 )
      theOutputDestination . write ( "\n" );      

    theOutputDestination . write ( "#ifndef " );
    theOutputDestination . write ( theNameString );
    theOutputDestination
      . write ( DataComponent.CXX_DISTRIBUTED_FORMAT_TRAILER );

    theOutputDestination . write ( "\n#error  \"Error:  " );
    theOutputDestination . write ( theNameString );
    theOutputDestination
      . write ( DataComponent.CXX_DISTRIBUTED_FORMAT_TRAILER );

    theOutputDestination . write ( " is not #define'd!\"\n#endif /* " );
    theOutputDestination . write ( theNameString );
    theOutputDestination
      . write ( DataComponent.CXX_DISTRIBUTED_FORMAT_TRAILER );

    theOutputDestination . write ( " */" );

    theOutputDestination . addIndent ( indent );
    theOutputDestination . write ( "\n" );
  }


  private static DataVector taskArgumentsVector
			     = new DataVector ( 100 /*arbitrary large size*/ );

  public synchronized static String getDistributedFormatString (
					    DataTaskArgument theTaskArgument,
					    DataComponent    theSource,
					    String           theErrorLocation )
  {
    taskArgumentsVector . removeAllElements();
    taskArgumentsVector . addElement ( theTaskArgument );

    return DataTaskArgument.getDistributedFormatString (
              taskArgumentsVector,
	      theSource,
	      theErrorLocation,
	      null,
	      null );
  }

  public synchronized static String getDistributedFormatString (
				      DataVector    theTaskArgumentsVector,
				      DataComponent theSource,
				      String        theErrorLocation,
				      String        thePreString,
				      String        thePostString )
  {
    boolean forceDynamicString = false;

    if ( theTaskArgumentsVector . size() <= 0 )
    {
      return "(STRING(NULL))";
    }


	/* Do we need a string buffer? */
    for ( int i=0;   i < theTaskArgumentsVector . size();   i++ )
    {
      if ( ((DataTaskArgument) (theTaskArgumentsVector . elementAt ( i )))
	      . getDistributedFormatRequiresDynamicString() )

      {
	forceDynamicString = true;
	break;
      }
    }

    return getDistributedFormatString ( theTaskArgumentsVector,
					theSource,
					theErrorLocation,
					thePreString,
					thePostString,
					forceDynamicString );
  }

  public static String getDistributedFormat_DynamicStringBufferLead()
  {
    return DataComponent.CXX_DYNAMIC_STRING_BUFFER + "() << ";
  }

  public synchronized static String getDistributedFormatString (
				     DataVector     theTaskArgumentsVector,
				     DataComponent  theSource,
				     String         theErrorLocation,
				     String         thePreString,
				     String         thePostString,
				     boolean        theForceDynamicString  )
  {
    StringBuffer    stringBuffer = new StringBuffer ( 1000 /*Arbitrary size*/);
    int             i;
    String          distributedFormatString;


	/* Do we need a dynamic string buffer? */
    if ( theForceDynamicString )
    {
      stringBuffer . append (getDistributedFormat_DynamicStringBufferLead());
    }

     /* If we need a leading string, it must go after the dynamic string bit */
    if ( thePreString != null )
      stringBuffer . append ( thePreString );

	/* Start our string. */
    stringBuffer . append ( "\"" );

	/* Add each element of theTaskArgumentsVector */
    for ( i=0;   i < theTaskArgumentsVector . size();   i++ )
    {
      distributedFormatString
	= ((DataTaskArgument) (theTaskArgumentsVector . elementAt ( i )))
	     . getDistributedFormatString();

      if ( distributedFormatString == null )
      {
	throw new CompilationException (
	   theSource.getMessageFilenameLead() + theSource.getLineNumberString()
	   + ":  Error: Format constraint required.  Unable to automatically "
	   + "create distributed IPC argument-format string for "
	   + theErrorLocation 
	   + " element/argument \""
	   + ((DataTaskArgument) (theTaskArgumentsVector . elementAt ( i )))
                . toString ( true ) . trim()
	   + "\"." );
      }

      if ( i > 0 )
	stringBuffer . append ( "," );

      stringBuffer . append ( distributedFormatString );
    }

    stringBuffer . append ( "\"" );

	/* Add any post-string */
    if ( thePostString != null )
      stringBuffer . append ( thePostString );

    return stringBuffer.toString();

  } /* public synchronized static String getDistributedFormatString ( ... ) */





	/* Instance Data */
  protected boolean        isPersistentDeclaration;
  protected int            argumentDirection;
  protected String         argumentName;

	/* Used to specify DISTRIBUTED format string on a per-argument basis */
  protected DataExpression formatExpression;

	/* Special case:  DISTRIBUTED'able arguments */
  protected int                   distributedType;
  protected String                distributedId;
  protected boolean               distributedIsPointer;
  protected DataVector            distributedArrayExpressionsVector;
  protected DataDistributedStruct dataDistributedStruct;
  protected DataDistributedEnum   dataDistributedEnum;


	/* Instance Methods */
  public DataTaskArgument ( )
  {
    isPersistentDeclaration = false;
    setArgumentDirection ( DataTaskArgument . DEFAULT_DIRECTION );
    setArgumentNameWithoutParsingChecks ( DataComponent . EMPTY_STRING );

    formatExpression                  = null;
    distributedType                   = DataComponent.INVALID_INDEX;
    distributedId                     = null;
    distributedIsPointer              = false;
    distributedArrayExpressionsVector = null;
    dataDistributedStruct             = null;
    dataDistributedEnum               = null;
  }

  public boolean getIsPersistentDeclaration()
					    { return isPersistentDeclaration; }
  public void    setIsPersistentDeclaration(
					  boolean theIsPersistentDeclaration )
		      { isPersistentDeclaration = theIsPersistentDeclaration; }


  public int  getArgumentDirection ( ) { return argumentDirection; }
  public void setArgumentDirection ( int theArgumentDirection )
  {
    if (   ( theArgumentDirection != DataTaskArgument . IN_DIRECTION    )
	&& ( theArgumentDirection != DataTaskArgument . OUT_DIRECTION   )
	&& ( theArgumentDirection != DataTaskArgument . INOUT_DIRECTION )
	&& ( theArgumentDirection != DataTaskArgument . NO_DIRECTION    ) )
    {
      System.err.println ( "[DataTaskArgument]  Error:  "
			   + "Invalid Argument Direction ("
			   + theArgumentDirection + ").  Assuming default." );
      argumentDirection = DataTaskArgument . DEFAULT_DIRECTION;
    }
    else
    {
      argumentDirection = theArgumentDirection;
    }
  }


  public String getArgumentDirectionString ( )
  {
    return DataTaskArgument . ARGUMENT_TYPES [ getArgumentDirection() ];
  }

  public void   setArgumentDirectionString ( String theArgumentDirection )
  {
    int   newDirection = DataTaskArgument . getArgumentDirectionOfString (
							theArgumentDirection );

    if ( newDirection == DataTaskArgument . INVALID_DIRECTION )
    {
      System.err.println ( "[DataTaskArgument:setArgumentDirectionString]  "
			   + "Warning:  Invalid direction (\""
			   + theArgumentDirection
			   + "\".  Using default direction instead." );
      newDirection = DataTaskArgument . DEFAULT_DIRECTION;
    }

    setArgumentDirection ( newDirection );
  }


  public  String getArgumentName ( ) { return argumentName; }

  protected void setArgumentNameWithoutParsingChecks ( String theArgumentName )
				     { argumentName = theArgumentName; }

  public    void setArgumentName ( String theArgumentName )
    throws DetailedParseException
  {
	/* Special case blank/empty name */
    if ( DataComponent . isEmptyString ( theArgumentName ) )
    {
      setArgumentNameWithoutParsingChecks ( DataComponent.EMPTY_STRING );
      return;
    }

	/* General case -- parse the name */
    TDLParser . reinitParser ( theArgumentName );
    try
    {
      setArgumentNameWithoutParsingChecks ( 
			       TDLParser . getParser() . parseArgumentName() );
    }
    catch ( Throwable  theExceptionOrError )
    {
      didParseOfSubpartFail ( theExceptionOrError );
    }
  }


  public boolean hasEquals()
  {
    return hasIndex ( DataTaskArgument.ARGUMENT_EQUALS_INDEX );
  }

  public int countSubcomponentNewlinesAfterEquals()
  {
    return countSubcomponentNewlines (
		       getIndex ( DataTaskArgument.ARGUMENT_EQUALS_INDEX ) + 1,
		       getSubcomponentsCount(),
		       DataComponent.ENTIRE_OBJECT,
		       true /* Stop at first non-whitespace character */ );
  }



  DataExpression getFormatExpression() { return formatExpression; }
  void           setFormatExpression( DataExpression theFormatExpression )
			        { formatExpression = theFormatExpression; }



	/* Data relating to DISTRIBUTED'able arguments */
  public boolean getIsDistributed ()
  {
    return ( getDistributedType() >= DataTaskArgument.FIRST_TYPE   )
       &&  ( getDistributedType() <= DataTaskArgument.LAST_TYPE    );
  }

  public int     getDistributedType () { return distributedType; }
  public void    setDistributedType ( int    theDistributedType )
			 { distributedType = theDistributedType; }

  public String  getDistributedTypeString ()
  {
    if (   (    getDistributedType()
	     >= 0
	    )
	&& (    getDistributedType()
	     <  DataTaskArgument.DISTRIBUTED_TYPE_STRINGS.length
	    )
	)
      return DataTaskArgument.DISTRIBUTED_TYPE_STRINGS [ getDistributedType()];
    else
      return "UNKNOWN_TYPE: " + getDistributedType();
  }


  public String  getDistributedId () { return distributedId; }
  public void    setDistributedId ( String theDistributedId )
			 { distributedId = theDistributedId; }
  public String  getDistributedIdTranslatingScoping()
			 { return getDistributedId() . replace ( ':', '_' ); }


  public boolean getDistributedIsPointer () { return distributedIsPointer; }
  public void    setDistributedPointer ( boolean  theDistributedIsPointer )
			 { distributedIsPointer = theDistributedIsPointer; }

  public DataVector getDistributedArrayExpressionsVector()
				  { return distributedArrayExpressionsVector; }

  public boolean getDistributedHasArrayExpressions()
  {
    return ( getDistributedArrayExpressionsVector()          != null )
       &&  ( getDistributedArrayExpressionsVector() . size()  > 0    );
  }

  public void    addDistributedArrayExpression ( DataExpression theExpression )
  {
    if ( distributedArrayExpressionsVector == null )
      distributedArrayExpressionsVector
	= new DataVector ( 10 /*Arbitrary number*/ );

    distributedArrayExpressionsVector . addElement ( theExpression );
  }


  public void setDistributedStructNotSubcomponent (
					      DataDistributedStruct theStruct )
  {
    if ( dataDistributedStruct != null )
    {
      System.err.println (
	"[DataTaskArgument:setDistributedStruct]  Warning:  "
	+ "Internal programmer error encountered:  Had previously set "
	+ "the dataDistributedStruct object." );
    }

	/* So we can do the format-string thing. */
    dataDistributedStruct = theStruct;
  }

  public void setDistributedStruct ( DataDistributedStruct theStruct )
  {
    setDistributedStructNotSubcomponent ( theStruct );

	/* So we can trivial generate outselves... */
    addSubcomponent ( theStruct );
  }

  public DataDistributedStruct getDataDistributedStruct()
  {
    return dataDistributedStruct;
  }

  public void setDistributedEnumNotSubcomponent ( DataDistributedEnum theEnum )
  {
    if ( dataDistributedEnum != null )
    {
      System.err.println (
	"[DataTaskArgument:setDistributedEnum]  Warning:  "
	+ "Internal programmer error encountered:  Had previously set "
	+ "the dataDistributedEnum object." );
    }

	/* So we can do the format-string thing. */
    dataDistributedEnum = theEnum;
  }

  public void setDistributedEnum ( DataDistributedEnum theEnum )
  {
    setDistributedEnumNotSubcomponent ( theEnum );

	/* So we can trivial generate outselves... */
    addSubcomponent ( theEnum );
  }

  public DataDistributedEnum getDataDistributedEnum()
  {
    return dataDistributedEnum;
  }



  public void writeDistributedMacroRequirements (
					DataDestination  theOutputDestination )
  {
    DataTaskArgument.writeDistributedMacroRequirements ( theOutputDestination,
							 this,
							 null );
  }

  public boolean getDistributedFormatRequiresDynamicString()
  {
    if ( getFormatExpression() != null )
      return true;

    if ( getDistributedHasArrayExpressions() )
      return true;

    if (   (   ( getDistributedType() == DataTaskArgument.IDENTIFIER )
	    || ( getDistributedType() == DataTaskArgument.STRUCT     )
	    || ( getDistributedType() == DataTaskArgument.ENUM       ) )
	&& (     getDistributedId()   != null                          ) )
      return true;

    
    if ( getDistributedType() == DataTaskArgument.DATA_DISTRIBUTED_STRUCT )
    {
      if (   ( getDataDistributedStruct() != null )
	  && ( getDataDistributedStruct()
	         . getDistributedFormatRequiresDynamicString() == true ) )
	return true;
    }

    return false;
  }


  public String  getDistributedFormatString()
  {
    int          i;
    StringBuffer returnValue = new StringBuffer ( 100 /* Arbitrary */ );

    if ( getFormatExpression() != null )
      return (   "\" << "
	       + getFormatExpression() . toString ( true ) . trim()
	       + " << \"" );

    if ( getIsDistributed() != true )
      return null;

	/* Unsupported by IPC, as of yet... */
    if (   ( getDistributedType() == DataTaskArgument.INT8   )
	|| ( getDistributedType() == DataTaskArgument.U_INT8 ) )
      return null;

    if (   getDistributedType()
	 < DataTaskArgument.DISTRIBUTED_ARGUMENT_FORMAT_STRINGS.length )
      returnValue
	. append ( DataTaskArgument.DISTRIBUTED_ARGUMENT_FORMAT_STRINGS
	            [ getDistributedType() ] );

    else if (   (   ( getDistributedType() == DataTaskArgument.IDENTIFIER )
		 || ( getDistributedType() == DataTaskArgument.STRUCT     )
		 || ( getDistributedType() == DataTaskArgument.ENUM       ) )
	     && (     getDistributedId()   != null                          ) )
    {
      returnValue . append ( "\" << (" );
      returnValue . append ( getDistributedIdTranslatingScoping() );
      returnValue . append ( DataComponent.CXX_DISTRIBUTED_FORMAT_TRAILER );
      returnValue . append ( ") << \"" );
    }

    else if ( getDistributedType() == DataTaskArgument.DATA_DISTRIBUTED_STRUCT)
    {
      if ( getDataDistributedStruct() != null )
	returnValue . append ( getDataDistributedStruct()
			         . getDistributedFormatString (
				     false /*No Quotes*/,
				     false /*No Dynamic String Buffer*/ ) );
      else
	System.err.println (
	  "[DataTaskArgument:getDistributedFormatString]  Warning: "
	  + "Internal programmer error encountered:  No "
	  + "DataDistributedStruct object available." );
    }

    else if ( getDistributedType() == DataTaskArgument.DATA_DISTRIBUTED_ENUM)
    {
      if ( getDataDistributedEnum() != null )
	returnValue . append ( getDataDistributedEnum()
		                 . getDistributedFormatString (
				     false /*No Quotes*/,
				     false /*No Dynamic String Buffer*/ ) );
      else
	System.err.println (
	  "[DataTaskArgument:getDistributedFormatString]  Warning: "
	  + "Internal programmer error encountered:  No "
	  + "DataDistributedEnum object available." );
    }

	/************************************/
	/* Deal with pointers and arrays... */
	/************************************/

    if ( returnValue . length() > 0 )
    {
	/* Pointer */
      if ( getDistributedIsPointer() )
      {
	returnValue . insert ( 0, "*" );
      }

	/* Array */
      if ( getDistributedHasArrayExpressions() )
      {
	returnValue . insert ( 0, "[" );
	returnValue . append ( ":" );

	for ( i=0;   i < getDistributedArrayExpressionsVector().size();   i++ )
	{
	  if ( i > 0 )
	    returnValue . append ( "," );

	  returnValue . append ( "\" << " );
	  returnValue . append ( getDistributedArrayExpressionsVector()
				   . elementAt ( i ) );
	  returnValue . append ( " << \"" );
	}

	returnValue . append ( "]" );

      } /* if ( getDistributedHasArrayExpressions() ) */


      return returnValue . toString();

    } /* if ( returnValue . length() > 0 ) */

    else
    {
      return null;
    }
  }



  public String getWarnString ( int theObjectSubset )
  {
    return 
      super . getWarnString ( theObjectSubset )
      + " or DataTaskArgument.TYPE_NAME_AND_EQUALS ("
      + DataTaskArgument.TYPE_NAME_AND_EQUALS + ")"
      + " or DataTaskArgument.TYPE_AND_NAME ("
      + DataTaskArgument.TYPE_AND_NAME + ")"
      + " or DataTaskArgument.AFTER_EQUALS ("
      + DataTaskArgument.AFTER_EQUALS + ")"
      + " or DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS ("
      + DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS + ")"
      + " or DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT ("
      + DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT + ")";
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    if (   ( theObjectSubset == DataTaskArgument.TYPE_NAME_AND_EQUALS  )
	|| ( theObjectSubset == DataTaskArgument.TYPE_AND_NAME         )
	|| ( theObjectSubset == DataTaskArgument.AFTER_EQUALS          )
	|| ( theObjectSubset ==
		       DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS )
	|| ( theObjectSubset == 
		       DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT )
	)
      return true;
    else
      return super . isValidObjectSubset ( theObjectSubset );
  }


  public boolean isValid ( int theObjectSubsetToValidate )
  {
	/* Should probably have some way of checking to make sure a type
	 * has been set.   But short of re-parsing the object...
	 */

    if ( DataComponent . isEmptyString ( getArgumentName() ) )
      return false;
    else
      return true;
  }


  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
      /* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );


	/* If we are not doing a DataTaskArgument-specific generation.    *
	 * E.g. We may be generating the argument-direction or Persistent *
	 * declaration.  (AKA: ENTIRE_OBJECT or Cxx-Subset.)              */
    if (   (theObjectSubsetToGenerate != DataTaskArgument.TYPE_NAME_AND_EQUALS)
        && (theObjectSubsetToGenerate != DataTaskArgument.TYPE_AND_NAME       )
        && (theObjectSubsetToGenerate != DataTaskArgument.AFTER_EQUALS        )
        && (theObjectSubsetToGenerate
                    != DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS        )
        && (theObjectSubsetToGenerate
                    != DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT )
        )
    {
	/* Initialize us to generate non-significant tokens... */
      initializeGenerateSubcomponentIndex();

	/* Write any pre-first-token non-significant tokens */
      generateSubcomponents ( DataComponent.FIRST_TOKEN_INDEX,
			      theOutputDestination,
			      DataComponent.ENTIRE_OBJECT, false );

	/* Lets only do this if we are a PERSISTENT declaration */
      if ( getIsPersistentDeclaration() == true )
      {
	  /* Write any pre-arg-direction non-significant tokens */
	generateSubcomponents ( DataTaskArgument.PERSISTENT,
				theOutputDestination,
				DataComponent.ENTIRE_OBJECT, false );

	  /* Write the argument-direction */
	theOutputDestination . write ( DataTaskArgument.PERSISTENT );
      }

	/* Lets only do this if we have an argument-direction!!! */
      if ( getArgumentDirection() != DataTaskArgument.NO_DIRECTION )
      {
	  /* Write any pre-arg-direction non-significant tokens */
	generateSubcomponents ( DataTaskArgument.ARGUMENT_DIRECTION_INDEX,
				theOutputDestination,
				DataComponent.ENTIRE_OBJECT, false );

	  /* Write the argument-direction */
	theOutputDestination . write ( getArgumentDirectionString() );
      }
    }
    else /* E.g.: DataTaskArgument specific Generation type.  *
	  *       No PERSISTENT keyword or argument-direction */
    {
	/* Initialize us to generate non-significant tokens... */
      initializeGenerateSubcomponentIndex (
				   DataTaskArgument.ARGUMENT_DIRECTION_INDEX );
    }


	/***************************************/
	/* Standard straightforward Generation */
	/***************************************/

    if ( theObjectSubsetToGenerate != DataTaskArgument.AFTER_EQUALS )
    {
	/* Write any pre-arg-name (which includes the arg-type) tokens */
      generateSubcomponents ( DataTaskArgument.ARGUMENT_NAME_INDEX,
			      theOutputDestination,
			      DataComponent.ENTIRE_OBJECT, false );

	/* write the argument-name */
      theOutputDestination . write ( getArgumentName() );
    }


	/* This isn't pretty.  It's redundant coding.  A poor practice.     *
	 * But it sure makes it a lot easier to figure out what's going on. */
    switch ( theObjectSubsetToGenerate )
    {
      case DataTaskArgument.TYPE_NAME_AND_EQUALS:
	generateAllRemainingSubcomponents ( theOutputDestination,
					    DataComponent.ENTIRE_OBJECT,
					    false );
	break;


      case DataTaskArgument.TYPE_AND_NAME:
	if ( hasEquals() )
	  generateSubcomponents ( DataTaskArgument.ARGUMENT_EQUALS_INDEX,
				  theOutputDestination,
				  DataComponent.ENTIRE_OBJECT, false );
	else
	  generateAllRemainingSubcomponents ( theOutputDestination,
					      DataComponent.ENTIRE_OBJECT,
					      false );
	break;


      case DataTaskArgument.AFTER_EQUALS:
	initializeGenerateSubcomponentIndex (
		     getIndex ( DataTaskArgument.ARGUMENT_EQUALS_INDEX ) + 1 );

	generateAllRemainingSubcomponents ( theOutputDestination,
					    DataComponent.ENTIRE_OBJECT,
					    false );
	break;


      case DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS:
      case DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT:

	if ( hasEquals() )
	{
	  generateSubcomponents ( DataTaskArgument.ARGUMENT_EQUALS_INDEX,
				  theOutputDestination,
				  DataComponent.ENTIRE_OBJECT, false );

	  theOutputDestination . write ( " /* " );
	  theOutputDestination . setIsNestingComments ( true );

	  if ( hasIndex ( DataTaskArgument.FORMAT ) )
	    generateSubcomponents ( DataTaskArgument.FORMAT,
				    theOutputDestination,
				    DataComponent.ENTIRE_OBJECT, false );
	  else
	    generateAllRemainingSubcomponents ( theOutputDestination,
						DataComponent.ENTIRE_OBJECT,
						false );

	  theOutputDestination . setIsNestingComments ( false );
	  theOutputDestination . write ( " */" );
	}

	if (    theObjectSubsetToGenerate
	     == DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS )
	{
	  generateAllRemainingSubcomponents ( theOutputDestination,
					      DataComponent.ENTIRE_OBJECT,
					      false );
	  break;
	}

	/* NO BREAK FOR TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT */


      default:

	if ( getFormatExpression() != null )
	{
	  generateSubcomponents ( DataTaskArgument.FORMAT,
				  theOutputDestination,
				  DataComponent.ENTIRE_OBJECT, false );

	  if ( theObjectSubsetToGenerate != DataComponent.ENTIRE_OBJECT )
	  {
	    theOutputDestination . write ( " /* " );
	    theOutputDestination . setIsNestingComments ( true );
	  }

	  theOutputDestination . write ( DataTaskArgument.FORMAT );

	  generateSubcomponents ( DataTaskArgument.FORMAT_EXPRESSION,
				  theOutputDestination,
				  DataComponent.ENTIRE_OBJECT, false );

	  getFormatExpression() . generate ( theOutputDestination,
					     DataComponent.ENTIRE_OBJECT );

	  if ( theObjectSubsetToGenerate != DataComponent.ENTIRE_OBJECT )
	  {
	    theOutputDestination . setIsNestingComments ( false );
	    theOutputDestination . write ( " */" );
	  }
	} /* if ( getFormatExpression() != null ) */

	generateAllRemainingSubcomponents ( theOutputDestination,
					    DataComponent.ENTIRE_OBJECT,
					    false );
	break;

    } /* switch ( theObjectSubsetToGenerate ) */

  } /* public void generate ( ... ) */



	/* Testing / Debugging scaffolding code. */
  public static void main ( String[] args )
    throws java.io.IOException, DetailedParseException
  {
// Suggested test string:
// echo '/*1*/ int /*2*/ * /*3*/ * /*4*/ ijk /*5*/ = /*6*/ a /*7*/ + /*8*/ b /*9*/ format /*10*/ AC /*11*/ AB /*12*/' | java DataTaskArgument
// ( ... | cksum ; echo '859967355 1250' ) | uniq -c

    StringBuffer       inputData = new StringBuffer ( 1000 );
    int                inchar;
    DataTaskArgument   dataTaskArgument = null;

    while ( ( inchar = System.in.read() ) != -1 )
      inputData . append ( (char) ( inchar & 0x00ff ) );

    System.out.println ( "Testing:                                 "
			 + inputData.toString() + "" );


    TDLParser . getParser() . reinitParser ( inputData.toString() );
    try
    {
      dataTaskArgument = TDLParser . getParser()
				   . parseTaskArgument_AndTrailingEOF ( null );
    } catch ( Throwable  theExceptionOrError )
    {
        /* Deal with any bizarre errors that crop up */
      DataComponent.didParseOfSubpartFail ( theExceptionOrError );
    }
    

    System.out.println ( "DataComponent.ENTIRE_OBJECT:             "
      + dataTaskArgument.toString ( DataComponent.ENTIRE_OBJECT ) );

    System.out.println ( "DataComponent.CXX_HEADER:                "
      + dataTaskArgument.toString ( DataComponent.CXX_HEADER ) );

    System.out.println ( "DataComponent.CXX_CODE:                  "
      + dataTaskArgument.toString ( DataComponent.CXX_CODE ) );

    System.out.println ( "TYPE_NAME_AND_EQUALS:                    "
      + dataTaskArgument.toString ( DataTaskArgument.TYPE_NAME_AND_EQUALS ) );

    System.out.println ( "TYPE_AND_NAME:                           "
      + dataTaskArgument.toString ( DataTaskArgument.TYPE_AND_NAME ) );

    System.out.println ( "AFTER_EQUALS:                            "
      + dataTaskArgument.toString ( DataTaskArgument.AFTER_EQUALS ) );

    System.out.println ( "TYPE_NAME_AND_COMMENTED_EQUALS:          "
      + dataTaskArgument.toString ( DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS ) );

    System.out.println ( "TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT:   "
      + dataTaskArgument.toString ( DataTaskArgument.TYPE_NAME_AND_COMMENTED_EQUALS_FORMAT ) );
  }
}
