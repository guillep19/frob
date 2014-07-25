/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

public class DataConstraint extends DataComponent
			    implements DataValidateCode,
				       DataConstraintTagTaskIndexes
{
    /* Class Variables/Methods */

	/* generate() Constants. */
  public final static String  BEGIN_TAG_TASK_INDEX
			 = DataConstraintTagTaskIndexes.BEGIN_TAG_TASK_INDEX;
  public final static String  DEFAULT_TAG_TASK_INDEX
			 = DataConstraintTagTaskIndexes.DEFAULT_TAG_TASK_INDEX;
  public final static String  END_TAG_TASK_INDEX
			 = DataConstraintTagTaskIndexes.END_TAG_TASK_INDEX;
  public final static String  TIME_SEPARATOR             = ":";
  public final static String  FRACTIONAL_TIME_SEPARATOR  = ".";

	/* Non-Significant token indexes */
  public final static String  TAG_TASK = DataConstraintTagTaskIndexes.TAG_TASK;
  public final static String  KEYWORD           = "K";
  public final static String  CONSTRAINT_OPTION = "ConstraintOption";
  public final static String  EVENT_CONSTRAINT_OPTION
					        = "EventConstraintOption";
  public final static String  STATE_BOUNDARY    = "StateBoundary";
  public final static String   PRE_TIME_HOURS   =  "PreTimeHours";
  public final static String  POST_TIME_HOURS   = "PostTimeHours";
  public final static String   PRE_TIME_MINUTES =  "PreTimeMinutes";
  public final static String  POST_TIME_MINUTES = "PostTimeMinutes";
  public final static String   PRE_TIME_SECONDS =  "PreTimeSeconds";
  public final static String  POST_TIME_SECONDS = "PostTimeSeconds";
  public final static String   PRE_TIME_FRACTIONS_OF_A_SECOND
						=  "PreTimeFractionsOfASecond";


	/* String Constants */
  public final static String  CHILD    = "CHILD";
  public final static String  THIS     = DataComponent.THIS;
  public final static String  PREVIOUS = "PREVIOUS";


	/* Generic invalid index */
  public final static int INVALID                = -1;

	/* Constraints: */
  public final static int EXPAND_FIRST           = 0;
  public final static int DELAY_EXPANSION        = 1;
  public final static int SEQUENTIAL_HANDLING    = 2;
  public final static int SEQUENTIAL_EXPANSION   = 3;
  public final static int SEQUENTIAL_EXECUTION   = 4;
  public final static int SERIAL                 = 5;
  public final static int PARALLEL               = 6;
  public final static int WAIT                   = 7;
  public final static int DISABLE_UNTIL_EVENT    = 8;
  public final static int DISABLE_UNTIL_TIME     = 9;
  public final static int DISABLE_FOR_TIME       = 10;
  public final static int TERMINATE_AT_EVENT     = 11;
  public final static int TERMINATE_AT_TIME      = 12;
  public final static int TERMINATE_IN_TIME      = 13;
  public final static int TERMINATE              = 14;
  public final static int ACTIVATE_AT_EVENT      = 15;
  public final static int ACTIVATE_AT_TIME       = 16;
  public final static int ACTIVATE_IN_TIME       = 17;
  public final static int ACTIVATE               = 18;
  public final static int MAXIMUM_ACTIVATE       = 19;
  public final static int MAXIMUM_TRIGGER        = 20;
  public final static int MONITOR_PERIOD         = 21;
  public final static int EXCEPTION_HANDLER      = 22;
  public final static int ON_TERMINATE           = 23;
  public final static int DISTRIBUTED_FORMAT     = 24;
  public final static int ON_AGENT               = 25;
  public final static int TCM_TASK_TREE_PARENT   = 26;
  public final static int TDL_REF_IN             = 27;
  public final static int TCM_TASK_TREE_NAME     = 28;
  public final static int MAX_CONSTRAINT_INDEX   = TCM_TASK_TREE_NAME;

  public static int  getLastIndexOfConstraintOfType (
			      int                    theConstraintType,
			      DataConstrainedObject  theDataConstrainedObject )
  {
    for ( int i  = theDataConstrainedObject . getConstraintCount() - 1;
	      i >= 0;
	      i -- )
    {
      if (    theDataConstrainedObject . getConstraint(i) . getConstraintType()
	   == theConstraintType )
	return i;
    }

    return DataComponent.INVALID_INDEX;
  }

	/* These are all used during the task-spawning/insertion process. */
  public static boolean getShouldConstraintBeCxxGenerated (
					  DataConstraint theConstraint,
					  DataVector     theSetOfConstraints,
					  boolean        theIsPostInsert,
					  boolean        theIsInternalToTasks )
  {
    if (    DataConstraint.getIsPostInsertConstraint ( theConstraint )
	 != theIsPostInsert )
      return false;

    if ( DataConstraint.getIsConstraintOverriden ( theConstraint,
						   theSetOfConstraints ) )
      return false;

    if ( DataConstraint.getIsNonCxxGeneratingConstraint ( theConstraint ) )
      return false;

    return true;
  }


  public final static boolean[] IS_POST_INSERT_CONSTRAINT
				      = { false,  /* EXPAND_FIRST         */
					  false,  /* DELAY_EXPANSION      */
					  false,  /* SEQUENTIAL_HANDLING  */
					  false,  /* SEQUENTIAL_EXPANSION */
					  false,  /* SEQUENTIAL_EXECUTION */
					  false,  /* SERIAL               */
					  false,  /* PARALLEL             */
					  true,   /* WAIT                 */
					  false,  /* DISABLE_UNTIL_EVENT  */
					  false,  /* DISABLE_UNTIL_TIME   */
					  false,  /* DISABLE_FOR_TIME     */
					  false,  /* TERMINATE_AT_EVENT   */
					  false,  /* TERMINATE_AT_TIME    */
					  false,  /* TERMINATE_IN_TIME    */
					  true,   /* TERMINATE            */
					  false,  /* ACTIVATE_AT_EVENT    */
					  false,  /* ACTIVATE_AT_TIME     */
					  false,  /* ACTIVATE_IN_TIME     */
					  true,   /* ACTIVATE             */
					  false,  /* MAXIMUM_ACTIVATE     */
					  false,  /* MAXIMUM_TRIGGER      */
					  false,  /* MONITOR_PERIOD       */
					  false,  /* EXCEPTION_HANDLER    */
				          false,  /* ON_TERMINATE         */
				          false,  /* DISTRIBUTED_FORMAT   */
				          false,  /* ON_AGENT             */
				          false,  /* TCM_TASK_TREE_PARENT */
				          false,  /* TDL_REF_IN           */
				          false   /* TCM_TASK_TREE_NAME   */ };

  public static boolean getIsPostInsertConstraint (
					     DataConstraint theDataConstraint )
  {
    return DataConstraint.getIsPostInsertConstraint (
				     theDataConstraint . getConstraintType() );
  }

  public static boolean getIsPostInsertConstraint ( int theConstraintType )
  {
    if (    ( theConstraintType < 0 )
	 || ( theConstraintType > MAX_CONSTRAINT_INDEX ) )
    {
      System.err.println ( "[DataConstraint:getIsPostInsertConstraint]  "
			   + "Warning:  theConstraintType ("
			   + theConstraintType + ") is out of range..." );
      return false;
    }
    else
      return DataConstraint.IS_POST_INSERT_CONSTRAINT [ theConstraintType ];
  }



	/* Some constraints can be combined.  But OTHERS override one another.
	 * In which case, we need to detect the situation, and suppress them.
	 */
  public final static boolean[] IS_OVERRIDING_CONSTRAINT
				      = { false,  /* EXPAND_FIRST         */
					  false,  /* DELAY_EXPANSION      */
					  false,  /* SEQUENTIAL_HANDLING  */
					  false,  /* SEQUENTIAL_EXPANSION */
					  false,  /* SEQUENTIAL_EXECUTION */
					  false,  /* SERIAL               */
					  false,  /* PARALLEL             */
					  false,  /* WAIT                 */
					  false,  /* DISABLE_UNTIL_EVENT  */
					  false,  /* DISABLE_UNTIL_TIME   */
					  false,  /* DISABLE_FOR_TIME     */
					  false,  /* TERMINATE_AT_EVENT   */
					  false,  /* TERMINATE_AT_TIME    */
					  false,  /* TERMINATE_IN_TIME    */
					  false,  /* TERMINATE            */
					  false,  /* ACTIVATE_AT_EVENT    */
					  false,  /* ACTIVATE_AT_TIME     */
					  false,  /* ACTIVATE_IN_TIME     */
					  false,  /* ACTIVATE             */
					  true,   /* MAXIMUM_ACTIVATE     */
					  true,   /* MAXIMUM_TRIGGER      */
					  true,   /* MONITOR_PERIOD       */
					  false,  /* EXCEPTION_HANDLER    */
				          false,  /* ON_TERMINATE         */
				          true,   /* DISTRIBUTED_FORMAT   */
				          true,   /* ON_AGENT             */
				          true,   /* TCM_TASK_TREE_PARENT */
				          true,   /* TDL_REF_IN           */
				          true    /* TCM_TASK_TREE_NAME   */ };

  public static boolean getIsOverridingConstraint (
					     DataConstraint theDataConstraint )
  {
    return getIsOverridingConstraint( theDataConstraint . getConstraintType());
  }

  public static boolean getIsOverridingConstraint( int theConstraintType )
  {
    if (    ( theConstraintType < 0 )
	 || ( theConstraintType > MAX_CONSTRAINT_INDEX ) )
    {
      System.err.println ( "[DataConstraint:getIsOverridingConstraint]  "
			   + "Warning:  theConstraintType ("
			   + theConstraintType + ") is out of range..." );
      return false;
    }
    else
      return DataConstraint.IS_OVERRIDING_CONSTRAINT [ theConstraintType ];
  }

	/*     Returns true iff theConstraint is a overriding constraint
	 * AND there is another constraint of the same type in
	 *     theSetOfConstraints before theConstraint.
	 */
  public static boolean getIsConstraintOverriden (
					   DataConstraint theConstraint,
					   DataVector     theSetOfConstraints )
  {
    int             i;
    DataConstraint  currentDataConstraint;

    if ( getIsOverridingConstraint ( theConstraint ) == false )
    {
      return false;
    }

    for ( i = theSetOfConstraints.count() - 1;   i >= 0;   i-- )
    {
      currentDataConstraint
	= ( (DataConstraint) ( theSetOfConstraints . elementAt ( i ) ) );

      if ( currentDataConstraint == theConstraint )
      {
	return false;
      }

      if (     currentDataConstraint . getConstraintType()
	   ==  theConstraint         . getConstraintType() )
      {
	return true;
      }
    } /* FOR ( 0 <= i < theSetOfConstraints.count() ) */

    return false;
  }




  public final static boolean[] IS_NON_CXX_GENERATING_CONSTRAINT
				      = { false,  /* EXPAND_FIRST         */
					  false,  /* DELAY_EXPANSION      */
					  false,  /* SEQUENTIAL_HANDLING  */
					  false,  /* SEQUENTIAL_EXPANSION */
					  false,  /* SEQUENTIAL_EXECUTION */
					  false,  /* SERIAL               */
					  false,  /* PARALLEL             */
					  false,  /* WAIT                 */
					  false,  /* DISABLE_UNTIL_EVENT  */
					  false,  /* DISABLE_UNTIL_TIME   */
					  false,  /* DISABLE_FOR_TIME     */
					  false,  /* TERMINATE_AT_EVENT   */
					  false,  /* TERMINATE_AT_TIME    */
					  false,  /* TERMINATE_IN_TIME    */
					  false,  /* TERMINATE            */
					  false,  /* ACTIVATE_AT_EVENT    */
					  false,  /* ACTIVATE_AT_TIME     */
					  false,  /* ACTIVATE_IN_TIME     */
					  false,  /* ACTIVATE             */
					  false,  /* MAXIMUM_ACTIVATE     */
					  false,  /* MAXIMUM_TRIGGER      */
					  false,  /* MONITOR_PERIOD       */
					  false,  /* EXCEPTION_HANDLER    */
				          false,  /* ON_TERMINATE         */
				          true,   /* DISTRIBUTED_FORMAT   */
				          true,   /* ON_AGENT             */
				          true,   /* TCM_TASK_TREE_PARENT */
				          true,   /* TDL_REF_IN           */
				          true    /* TCM_TASK_TREE_NAME   */ };

  public static boolean getIsNonCxxGeneratingConstraint (
					     DataConstraint theDataConstraint )
  {
    return DataConstraint.getIsNonCxxGeneratingConstraint (
				     theDataConstraint . getConstraintType() );
  }

  public static boolean getIsNonCxxGeneratingConstraint (int theConstraintType)
  {
    if (    ( theConstraintType < 0 )
	 || ( theConstraintType > MAX_CONSTRAINT_INDEX ) )
    {
      System.err.println ( "[DataConstraint:getIsNonCxxGeneratingConstraint]  "
			   + "Warning:  theConstraintType ("
			   + theConstraintType + ") is out of range..." );
      return false;
    }
    else
      return
	DataConstraint.IS_NON_CXX_GENERATING_CONSTRAINT [ theConstraintType ];
  }




	/* This *should* only affect constraint-statements after a task
	 * has been spawned.
	 * 
	 * It is actually hard-coded into
	 * DataTaskDefinition.validateTaskForCxxGeneration().
	 *
	 * But it's also used for avoiding printing overriding messages
	 * for illegal overriding after-task-spawned constraints.
	 * (Which, at present is only ON_AGENT.)
	 */
  public final static boolean[] APPLYABLE_AFTER_TASK_SPAWNED
				      = { false,  /* EXPAND_FIRST         */
					  false,  /* DELAY_EXPANSION      */
					  false,  /* SEQUENTIAL_HANDLING  */
					  false,  /* SEQUENTIAL_EXPANSION */
					  false,  /* SEQUENTIAL_EXECUTION */
					  false,  /* SERIAL               */
					  false,  /* PARALLEL             */
					  true,   /* WAIT                 */
					  false,  /* DISABLE_UNTIL_EVENT  */
					  false,  /* DISABLE_UNTIL_TIME   */
					  false,  /* DISABLE_FOR_TIME     */
					  true,   /* TERMINATE_AT_EVENT   */
					  true,   /* TERMINATE_AT_TIME    */
					  true,   /* TERMINATE_IN_TIME    */
					  true,   /* TERMINATE            */
					  true,   /* ACTIVATE_AT_EVENT    */
					  true,   /* ACTIVATE_AT_TIME     */
					  true,   /* ACTIVATE_IN_TIME     */
					  true,   /* ACTIVATE             */
					  true,   /* MAXIMUM_ACTIVATE     */
					  true,   /* MAXIMUM_TRIGGER      */
					  true,   /* MONITOR_PERIOD       */
				/*???*/	  false,  /* EXCEPTION_HANDLER    */
				/*???*/	  false,  /* ON_TERMINATE         */
				          false,  /* DISTRIBUTED_FORMAT   */
				          false,  /* ON_AGENT             */
				          false,  /* TCM_TASK_TREE_PARENT */
				          false,  /* TDL_REF_IN           */
				          false   /* TCM_TASK_TREE_NAME   */ };

  public static boolean getIsApplyableAfterTaskSpawned (
					     DataConstraint theDataConstraint )
  {
    return DataConstraint.getIsApplyableAfterTaskSpawned (
				     theDataConstraint . getConstraintType() );
  }

  public static boolean getIsApplyableAfterTaskSpawned ( int theConstraintType)
  {
    if (    ( theConstraintType < 0 )
	 || ( theConstraintType > MAX_CONSTRAINT_INDEX ) )
    {
      System.err.println ( "[DataConstraint:getIsApplyableAfterTaskSpawned]  "
			   + "Warning:  theConstraintType ("
			   + theConstraintType + ") is out of range..." );
      return false;
    }
    else
      return DataConstraint.APPLYABLE_AFTER_TASK_SPAWNED [ theConstraintType ];
  }




  public final static String[] CONSTRAINT_NAMES = { "EXPAND_FIRST",
						    "DELAY_EXPANSION",
						    "SEQUENTIAL_HANDLING",
						    "SEQUENTIAL_EXPANSION",
						    "SEQUENTIAL_EXECUTION",
						    "SERIAL",
						    "PARALLEL",
						    "WAIT",
						    "DISABLE_UNTIL_EVENT",
						    "DISABLE_UNTIL_TIME",
						    "DISABLE_FOR_TIME",
						    "TERMINATE_AT_EVENT",
						    "TERMINATE_AT_TIME",
						    "TERMINATE_IN_TIME",
						    "TERMINATE",
						    "ACTIVATE_AT_EVENT",
						    "ACTIVATE_AT_TIME",
						    "ACTIVATE_IN_TIME",
						    "ACTIVATE",
						    "MAXIMUM_ACTIVATE",
						    "MAXIMUM_TRIGGER",
						    "MONITOR_PERIOD",
						    "EXCEPTION_HANDLER",
						    "ON_TERMINATE",
						    "FORMAT",
						    "ON_AGENT",
						    "TCM_TASK_TREE_PARENT",
						    "TDL_REF_IN",
						    "TCM_TASK_TREE_NAME" };

  public static int getConstraintIndexOfString ( String theConstraint )
  {
    for ( int i=0;  i < DataConstraint.CONSTRAINT_NAMES.length;  i++ )
    {
      if ( DataConstraint.CONSTRAINT_NAMES [ i ]
	   . equalsIgnoreCase ( theConstraint ) )
      {
	return i;
      }
    }

    System.err.println ( "[DataConstraint:getConstraintIndexOfString]  "
			 + "Warning:  Unknown Constraint \""
			 + theConstraint + "\"." );

    return DataConstraint.INVALID;
  }

  public static String getConstraintNameString ( int theConstraintType )
  {
    if (   ( theConstraintType >= 0                                      )
	&& ( theConstraintType <  DataConstraint.CONSTRAINT_NAMES.length ) )
      return DataConstraint.CONSTRAINT_NAMES [ theConstraintType ];
    else
      return "**UNKNOWN**";
  }





  public final static String[][]
     CONSTRAINT_STRINGS = { { "EXPAND",     "FIRST"               },
			    { "DELAY",      "EXPANSION"           },
			    { "SEQUENTIAL", "HANDLING"            },
			    { "SEQUENTIAL", "EXPANSION"           },
			    { "SEQUENTIAL", "EXECUTION"           },
			    { "SERIAL"                            },
			    { "PARALLEL"                          },
			    { "WAIT"                              },
			    { "DISABLE",    "UNTIL"               },
			    { "DISABLE",    "UNTIL"               },
			    { "DISABLE",    "FOR",        "AFTER" },
			    { "TERMINATE",  "AT",                 },
			    { "TERMINATE",  "AT",                 },
			    { "TERMINATE",  "IN",         "AFTER" },
			    { "TERMINATE"                         },
			    { "ACTIVATE",   "AT",                 },
			    { "ACTIVATE",   "AT",                 },
			    { "ACTIVATE",   "IN",         "AFTER" },
			    { "ACTIVATE"                          },
			    { "MAXIMUM",    "ACTIVATE"            },
			    { "MAXIMUM",    "TRIGGER"             },
			    { "PERIOD"                            },
			    { "EXCEPTION",  "HANDLER"             },
			    { "ON",         "TERMINATE",  "SPAWN" },
			    { "FORMAT"                            },
			    { "ON"                                },
			    { "PARENT"                            },
			    { "TDL_REF",    "IN"                  },
			    { "NAME"                              } };

  public static String[] getConstraintTokens ( int theConstraintIndex )
  {
    if (    ( theConstraintIndex < 0 )
	 || ( theConstraintIndex > MAX_CONSTRAINT_INDEX ) )
    {
      System.err.println ( "[DataConstraint:getConstraintStrings]  Warning:  "
			   + "theConstraintIndex (" + theConstraintIndex 
			   + ") is out of range..." );
      return null;
    }
    else
      return DataConstraint.CONSTRAINT_STRINGS [ theConstraintIndex ];
  }


	/* Constraint Options: */
  public final static int HANDLING    = 0;
  public final static int EXPANSION   = 1;
  public final static int PLANNING    = 1;
  public final static int EXECUTION   = 2;
  public final static int ACHIEVEMENT = 2;
  public final static int MAX_CONSTRAINT_OPTION_INDEX = EXECUTION;

  public final static String[]
     CONSTRAINT_OPTION_STRINGS   = { "HANDLING", "EXPANSION", "EXECUTION" };

  public final static String[]
     OLD_CONSTRAINT_OPTION_STRINGS = { "HANDLING", "PLANNING", "ACHIEVEMENT" };

  public static String getConstraintOptionString ( int theIndex )
  {
    if ( (theIndex < 0) || (theIndex > MAX_CONSTRAINT_OPTION_INDEX) )
    {
      System.err.println ( "[DataConstraint:getConstraintOptionString]  "
			   + "Warning:  theIndex (" + theIndex 
			   + ") is out of range..." );
      return null;
    }
    else
      return DataConstraint.CONSTRAINT_OPTION_STRINGS [ theIndex ];
  }

  public static int getConstraintOptionIndexOfString (
						   String theConstraintOption )
  {
    for ( int i=0;  i < DataConstraint.CONSTRAINT_OPTION_STRINGS.length;  i++ )
    {
      if ( DataConstraint.CONSTRAINT_OPTION_STRINGS [ i ]
	     . equalsIgnoreCase ( theConstraintOption ) )
      {
	return i;
      }
    }

    for ( int i=0;
	  i < DataConstraint.OLD_CONSTRAINT_OPTION_STRINGS.length;
	  i++ )
    {
      if ( DataConstraint.OLD_CONSTRAINT_OPTION_STRINGS [ i ]
	     . equalsIgnoreCase ( theConstraintOption ) )
      {
	return i;
      }
    }

    System.err.println ( "[DataConstraint:getConstraintOptionIndexOfString]  "
			 + "Warning:  Unknown Constraint-Option \""
			 + theConstraintOption + "\"." );

    return DataConstraint.INVALID;
  }



	/* State boundaries: */
  public final static int ENABLED   = 0;
  public final static int ACTIVE    = 1;
  public final static int COMPLETED = 2;
  public final static int MAX_STATE_BOUNDARY_INDEX = COMPLETED;

  public final static String[] STATE_BOUNDARY_STRINGS
					= { "ENABLED", "ACTIVE", "COMPLETED" };
  
  public static String getStateBoundaryString ( int theIndex )
  {
    if ( (theIndex < 0) || (theIndex > MAX_STATE_BOUNDARY_INDEX) )
    {
      System.err.println ( "[DataConstraint:getStateBoundaryString]  "
			   + "Warning:  theIndex (" + theIndex 
			   + ") is out of range..." );
      return null;
    }
    else
      return DataConstraint.STATE_BOUNDARY_STRINGS [ theIndex ];
  }

  public static int getStateBoundaryIndexOfString ( String theStateBoundary )
  {
    for ( int i=0;  i < DataConstraint.STATE_BOUNDARY_STRINGS.length;  i++ )
    {
      if ( DataConstraint.STATE_BOUNDARY_STRINGS [ i ]
	     . equalsIgnoreCase ( theStateBoundary ) )
      {
	return i;
      }
    }

    System.err.println ( "[DataConstraint:getStateBoundaryIndexOfString]  "
			 + "Warning:  Unknown State-Boundary \""
			 + theStateBoundary + "\"." );

    return DataConstraint.INVALID;
  }


	/* Time indexes */
  public final static int HOURS                        = 0;
  public final static int MINUTES                      = 1;
  public final static int SECONDS                      = 2;
  public final static int FRACTIONS_OF_A_SECOND        = 3;
  public final static int TOTAL_NUMBER_OF_TIME_INDEXES = 4;


	/* Generic useful function...
         * (Leading zeros would be interpreted as octal numbers in C++)
	 */
  public static String stripLeadingZeros ( String theString )
  {
    int i = 0;

    for (; (i < theString.length()) && (theString . charAt ( i ) == '0');  i++)
      ; /* EMPTY FOR BODY STATEMENT */

    if (   ( i <= 0                  )
	|| ( i >= theString.length() ) )
      return theString;

    else
      return theString . substring ( i );
  }



    /* Instance Variables */

		/* The actual type of this constraint */
  protected int      constraintType;

		/* The constrainer tag, if applicable... */
  protected String   eventTagTask;
  protected Object[] eventTagTaskIndexes;

		/* The constraint-option for this object */
  protected int      constraintOption;
		/* The constraint-option of the constraining object */
  protected int      eventConstraintOption;
		/* The state boundary of the constraining object */
  protected int      stateBoundary;

		/* The time, if applicable. */
  protected String[] time;

		/* The time, as an expression, if applicable... */
  protected DataExpression  timeExpression;

		/* A numeric expression value, if applicable. */
  protected DataExpression  numericExpression;

       /* Exception Handlers, which are treated as constraints for convenience,
	* need to have the form "<ID> ( args )".  The DataSpawnTask class
	* has all the features necessary to support this syntax.
	*/
  protected DataSpawnTask   exceptionHandlerTask;

       /* ON TERMINATE constraints have an associcated task that is run
	* if the preceeding task is terminated.  Rather than re-using
	* the exceptionHandlerTask, which would cause confusion,
	* we create another DataSpawnTask instance variable.
	*/
  protected DataSpawnTask   onTerminateTask;
  protected boolean         hasOnTerminateSpawnKeyword;

	/* DISTRIBUTED_FORMAT has its format-string argument.
	 * (That's treated as an concatenation-expression of
	 *  strings and macro-defines.)
	 */
  protected DataExpression  distributedFormatStringExpression;

	/* ON_AGENT can have its agent-name specified in a variety of ways,
	 * from a concatenation of strings and macros to a function call
	 * to a full-scale expression...
	 */
  protected DataExpression  distributedOnAgentExpression;

	/* TCM_TASK_TREE_PARENT should be "hard-to-use" to discourage
	 * casual use.  Therefore, the PARENT must be a TCM_Task_Tree_Ref,
	 * and can be specified any way the users wishes, including TDL_REF()
	 */
  protected DataExpression  tcmTaskTreeParentExpression;

	/* TDL_REF_IN, like TCM_TASK_TREE_PARENT, should be "hard-to-use" to
	 * discourage casual use.  Therefore, the TDL_REF_IN must be a
	 * TCM_Task_Tree_Ref, and can be specified any way the user desires.
	 */
  protected DataExpression  tdlRefInExpression;

	/* Allows users to specify a NAME for the corresponding
	 * TCM-Task-Tree node.  Overrides default.
	 * *MUST* evaluate to a (STRING) value.
	 */
  protected DataExpression  tcmTaskTreeNameExpression;


  protected String          cxxExternalSubtaskName;



  public DataConstraint ( )
  {
    clearConstraint();
  }

  public DataConstraint ( int  theConstraintType )
  {
    this();
    setConstraintType ( theConstraintType );
  }

  public void clearConstraint ( )
  {
    constraintType                    = DataConstraint.INVALID;
    eventTagTask                      = DataComponent.EMPTY_STRING;
    eventTagTaskIndexes               = null;
    constraintOption                  = DataConstraint.INVALID;
    eventConstraintOption             = DataConstraint.INVALID;
    stateBoundary                     = DataConstraint.INVALID;
    time                              = null;
    timeExpression                    = null;
    numericExpression                 = null;
    exceptionHandlerTask              = null;
    onTerminateTask                   = null;
    hasOnTerminateSpawnKeyword        = true;
    distributedFormatStringExpression = null;
    distributedOnAgentExpression      = null;
    tcmTaskTreeParentExpression       = null;
    tdlRefInExpression                = null;
    tcmTaskTreeNameExpression         = null;
    cxxExternalSubtaskName            = null;
  }


  public int  getConstraintType ( )  { return constraintType; }

  public void setConstraintType ( int theConstraintType )
					{ constraintType = theConstraintType; }

  public String getConstraintName ( )
    { return DataConstraint.getConstraintNameString ( getConstraintType() ); }


  public String[] getConstraintTypeTokens ( )
	{ return DataConstraint.getConstraintTokens ( getConstraintType() ); }

  public String   getTCMXorString ( )
  {
    switch ( getConstraintType() )
    {
      case DataConstraint.EXPAND_FIRST:
	return DataComponent.CXX_TCM_EXPAND_FIRST_XOR_VALUE;

      case DataConstraint.DELAY_EXPANSION:
	return DataComponent.CXX_TCM_DELAY_EXPANSION_XOR_VALUE;

      case DataConstraint.SEQUENTIAL_EXPANSION:
	return DataComponent.CXX_TCM_SEQUENTIAL_EXPANSION_XOR_VALUE;

      case DataConstraint.SEQUENTIAL_EXECUTION:
	return DataComponent.CXX_TCM_SEQUENTIAL_EXECUTION_XOR_VALUE;

      case DataConstraint.SERIAL:
	return DataComponent.CXX_TCM_SERIAL_XOR_VALUE;

      default:
	return DataComponent.CXX_TCM_BASE_XOR_VALUE;
    }
  }

  public boolean   getHasTCMXorString ( )
	  { return getTCMXorString() != DataComponent.CXX_TCM_BASE_XOR_VALUE; }


	/* Return true iff: This constraint can include an              *
	 *  expression-argument based off the current Task's arguments. *
	 *  (This in turn affects where DataTaskDefiniton can generate  *
	 *   this constraint.)                                          */
  public boolean   getNeedsTasksArguments ( )
  {
    switch ( getConstraintType() )
    {
      case DataConstraint.EXCEPTION_HANDLER:
      case DataConstraint.ON_TERMINATE:
      case DataConstraint.MAXIMUM_ACTIVATE:
      case DataConstraint.MAXIMUM_TRIGGER:
      case DataConstraint.MONITOR_PERIOD:
	return true;

      default:
	return false;
    }
  }


  public void setConstraintTypeStringWithoutParsing ( String theConstraintType)
  {
    int newConstraintType
	     = DataConstraint.getConstraintIndexOfString ( theConstraintType );

    if ( newConstraintType != DataConstraint.INVALID )
      setConstraintType ( newConstraintType );
    else
    {
      System.err.println (
	  "[DataConstraint:setConstraintTypeStringWithoutParsing]  Warning:  "
	  + "Unknown Constraint \"" + theConstraintType + "\"." );
    }
  }


  public boolean getHasEventTagTask ( )
  {
    return DataComponent.isEmptyString ( eventTagTask ) == false;
  }

  public boolean getHasNonStandardEventTagTask()
  {
    return ( getHasEventTagTask()           == true  )
       &&  ( getHasEventTagTaskOfThis()     == false )
       &&  ( getHasEventTagTaskOfChild()    == false )
       &&  ( getHasEventTagTaskOfPrevious() == false );
  }


  public boolean getHasEventTagTaskOfThis ( )
  {
    return getHasEventTagTask()
      && DataConstraint.THIS . equals ( getEventTagTask() );
  }

  public boolean getHasEventTagTaskOfChild ( )
  {
    return getHasEventTagTask()
      && DataConstraint.CHILD . equals ( getEventTagTask() );
  }

  public boolean getHasEventTagTaskOfPrevious ( )
  {
    return getHasEventTagTask()
      && DataConstraint.PREVIOUS . equals ( getEventTagTask() );
  }

  public String  getEventTagTask ( )
  {
    if ( eventTagTask != null )
      return eventTagTask;
    else
      return DataComponent.EMPTY_STRING;
  }

  public void    setEventTagTask  ( String theEventTagTask )
  {
    if ( theEventTagTask != null )
      eventTagTask = theEventTagTask;
    else
      eventTagTask = DataComponent.EMPTY_STRING;
  }

  public boolean getHasEventTagTaskIndexes ( )
  {
    return (   ( getEventTagTaskIndexes()          != null )
	    && ( getEventTagTaskIndexes() . length >  0    ) );
  }

  public Object[] getEventTagTaskIndexes () { return eventTagTaskIndexes; }

  public void     setEventTagTaskIndexesWithoutParsing (
					    Object[] theEventTagTaskIndexes )
  {
    eventTagTaskIndexes = theEventTagTaskIndexes;
  }


	/* DataConstraintTagTaskIndexes Interface */
  public boolean  getHasTagTask()       { return getHasEventTagTask();        }
  public String   getTagTask()          { return getEventTagTask();           }
  public void     setTagTask ( String theTagTask )
					{ setEventTagTask( theTagTask );      }
  public boolean  getHasTagTaskIndexes(){ return getHasEventTagTaskIndexes(); }
  public Object[] getTagTaskIndexes()   { return getEventTagTaskIndexes();    }
  public void     setTagTaskIndexesWithoutParsing ( Object[] theTagTaskIndexes)
		   { setEventTagTaskIndexesWithoutParsing(theTagTaskIndexes); }




  public boolean getHasConstraintOption ( )
  {
    return (   (    getConstraintOption() >= 0 )
	    && (    getConstraintOption()
		 <= DataConstraint.MAX_CONSTRAINT_OPTION_INDEX ) );
  }

  public int     getConstraintOption ( )  { return constraintOption; }

  public void    setConstraintOption ( int theConstraintOption )
				    { constraintOption = theConstraintOption; }

  public String  getConstraintOptionString ( )
  {
    return DataConstraint.getConstraintOptionString ( getConstraintOption() );
  }

  public void    setConstraintOptionStringWithoutParsing (
						   String theConstraintOption )
  {
    int newConstraintOption
		      = DataConstraint.getConstraintOptionIndexOfString (
							 theConstraintOption );

    if ( newConstraintOption != DataConstraint.INVALID )
      setConstraintOption ( newConstraintOption );
    else
    {
      System.err.println (
	 "[DataConstraint:setConstraintOptionStringWithoutParsing]  Warning:  "
	 + "Unknown Constraint \"" + theConstraintOption + "\"." );
    }
  }



  public boolean getHasEventConstraintOption ( )
  {
    return (   (    getEventConstraintOption() >= 0 )
	    && (    getEventConstraintOption()
		 <= DataConstraint.MAX_CONSTRAINT_OPTION_INDEX ) );
  }

  public int  getEventConstraintOption ( )
					{ return eventConstraintOption; }

  public void setEventConstraintOption ( 
					   int theEventConstraintOption )
	      { eventConstraintOption = theEventConstraintOption; }


  public String  getEventConstraintOptionString ( )
  {
    return DataConstraint.getConstraintOptionString (
					    getEventConstraintOption() );
  }

  public void    setEventConstraintOptionStringWithoutParsing (
					String theEventConstraintOption )
  {
    int newEventConstraintOption
		      = DataConstraint.getConstraintOptionIndexOfString (
					      theEventConstraintOption );

    if ( newEventConstraintOption != DataConstraint.INVALID )
      setEventConstraintOption ( newEventConstraintOption );
    else
    {
      System.err.println (
	"[DataConstraint:setEventConstraintOptionStringWithoutParsing]  "
	+ "Warning:  Unknown Constraint \""
	+ theEventConstraintOption + "\"." );
    }
  }




  public boolean getHasStateBoundary ( )
  {
    return(   ( getStateBoundary() >= 0 )
	   && ( getStateBoundary() <= DataConstraint.MAX_STATE_BOUNDARY_INDEX )
	   );
  }
  public int  getStateBoundary ( )  { return stateBoundary; }
  public void setStateBoundary ( int theStateBoundary )
					  { stateBoundary = theStateBoundary; }

  public String  getStateBoundaryString ( )
  {
    return DataConstraint.getStateBoundaryString ( getStateBoundary() );
  }

  public void  setStateBoundaryStringWithoutParsing ( String theStateBoundary )
  {
    int newStateBoundary
	   = DataConstraint.getStateBoundaryIndexOfString ( theStateBoundary );

    if ( newStateBoundary != DataConstraint.INVALID )
      setStateBoundary ( newStateBoundary );
    else
    {
      System.err.println (
	"[DataConstraint:setStateBoundaryStringWithoutParsing]  Warning:  "
	+ "Unknown State-Boundary \"" + theStateBoundary + "\"." );
    }
  }



  protected String[] getTime ( ) { return time; }

  protected boolean getHasTime ( )   { return getTime() != null; }
  protected void    setHasTime ( boolean theHasTime )
  {
    if ( theHasTime == getHasTime() )
      return;

    if ( theHasTime ) /* getHasTime() == false, time == null */
    {
      time = new String [ DataConstraint.TOTAL_NUMBER_OF_TIME_INDEXES ];
      for (int i=0;   i < DataConstraint.TOTAL_NUMBER_OF_TIME_INDEXES;   i++)
	time[i] = null;
    }
    else /* theHasTime == false, getHasTime() == true, time != null */
    {
      time = null;
    }
  }



  public boolean getHasHours ( )
  {
    return (   ( getHasTime() )
	    && ( DataComponent.isEmptyString ( 
					   getTime() [ DataConstraint.HOURS ] )
		 == false ) );
  }

  public String  getHours ( )
  {
    if ( getHasHours() )
      return getTime() [ DataConstraint.HOURS ];
    else
      return DataComponent.EMPTY_STRING;
  }

  public void setHoursWithoutParsing ( String theHours )
  {
    setHasTime ( true );
    getTime() [ DataConstraint.HOURS ] = theHours;
  }



  public boolean getHasMinutes ( )
  {
    return (   ( getHasTime() )
	    && ( DataComponent.isEmptyString ( 
					 getTime() [ DataConstraint.MINUTES ] )
		 == false ) );
  }

  public String getMinutes ( )
  {
    if ( getHasMinutes() )
      return getTime() [ DataConstraint.MINUTES ];
    else
      return DataComponent.EMPTY_STRING;
  }

  public void setMinutesWithoutParsing ( String theMinutes )
  {
    setHasTime ( true );
    getTime() [ DataConstraint.MINUTES ] = theMinutes;
  }



  public boolean getHasSeconds ( )
  {
    return (   ( getHasTime() )
	    && ( DataComponent.isEmptyString ( 
				         getTime() [ DataConstraint.SECONDS ] )
		 == false ) );
  }

  public String  getSeconds ( )
  {
    if ( getHasSeconds() )
      return getTime() [ DataConstraint.SECONDS ];
    else
      return DataComponent.EMPTY_STRING;
  }

  public void setSecondsWithoutParsing ( String theSeconds )
  {
    setHasTime ( true );
    getTime() [ DataConstraint.SECONDS ] = theSeconds;
  }



  public boolean getHasFractionsOfASecond ( )
  {
    return (   ( getHasTime() )
	    && ( DataComponent.isEmptyString ( 
			  getTime() [ DataConstraint.FRACTIONS_OF_A_SECOND ] )
		 == false ) );
  }

  public String  getFractionsOfASecond ( )
  {
    if ( getHasFractionsOfASecond() )
      return getTime() [ DataConstraint.FRACTIONS_OF_A_SECOND ];
    else
      return DataComponent.EMPTY_STRING;
  }

  public void setFractionsOfASecondWithoutParsing (
						 String theFractionsOfASecond )
  {
    setHasTime ( true );
    getTime() [ DataConstraint.FRACTIONS_OF_A_SECOND ] = theFractionsOfASecond;
  }



  public DataExpression getTimeExpression() { return timeExpression; }
  public void           setTimeExpression( DataExpression theTimeExpression )
					{ timeExpression = theTimeExpression; }



	/* For use with MAXIMUM_ACTIVATE & MAXIMUM_TRIGGER */
  public DataExpression  getNumericExpression() { return numericExpression; }
  public void    setNumericExpression( DataExpression theNumericExpression )
			       {  numericExpression = theNumericExpression; }


  public DataSpawnTask  getExceptionHandlerTask()
					       { return exceptionHandlerTask; }
  public boolean        setExceptionHandlerTask ( 
					DataSpawnTask theExceptionHandlerTask )
  {
    if (   (   ( theExceptionHandlerTask                            != null)
	    && ( theExceptionHandlerTask . getTaskName()            != null)
	    && ( theExceptionHandlerTask . getTaskName() . length() >  0   )
	    && ( theExceptionHandlerTask . getConstraintCount()     == 0   ) )
	|| (     theExceptionHandlerTask == null                             ))
    {
      exceptionHandlerTask = theExceptionHandlerTask;
      exceptionHandlerTask . setParent ( this );
      return true;
    }
    else
    {
      System.err.println ( "[DataConstraint:setExceptionHandlerTask]  Error: "
			   + "Bad object for theExceptionHandlerTask:  " 
			   + theExceptionHandlerTask . toString() );
      return false;
    }
  }


  public DataSpawnTask  getOnTerminateTask()  { return onTerminateTask; }
  public boolean        setOnTerminateTask ( 
					DataSpawnTask theOnTerminateTask )
  {
    if (   (   ( theOnTerminateTask                            != null)
	    && ( theOnTerminateTask . getTaskName()            != null)
	    && ( theOnTerminateTask . getTaskName() . length() >  0   )
	    && ( theOnTerminateTask . getConstraintCount()     == 0   ) )
	|| (     theOnTerminateTask == null                             ))
    {
      onTerminateTask = theOnTerminateTask;
      onTerminateTask . setParent ( this );
      if ( onTerminateTask . hasValidLineNumber() == false )
	onTerminateTask . setLineNumber ( getLineNumber() );
      return true;
    }
    else
    {
      System.err.println ( "[DataConstraint:setOnTerminateTask]  Error: "
			   + "Bad object for theOnTerminateTask:  " 
			   + theOnTerminateTask . toString() );
      return false;
    }
  }

  public void setLineNumber ( int theLineNumber )
  {
    super . setLineNumber ( theLineNumber );
    if ( getOnTerminateTask() != null )
      getOnTerminateTask() . setLineNumber ( getLineNumber() );
  }


  public boolean  getHasOnTerminateSpawnKeyword()
  {
    return hasOnTerminateSpawnKeyword;
  }

  public void     setHasOnTerminateSpawnKeyword (
					boolean theHasOnTerminateSpawnKeyword )
  {
    hasOnTerminateSpawnKeyword = theHasOnTerminateSpawnKeyword;
  }



  public DataExpression getDistributedFormatStringExpression()
	          { return distributedFormatStringExpression; }
  public void           setDistributedFormatStringExpression( 
						DataExpression theExpression )
			 { distributedFormatStringExpression = theExpression; }


  public DataExpression getDistributedOnAgentExpression()
				       { return distributedOnAgentExpression; }
  public void           setDistributedOnAgentExpression(
						DataExpression theExpression )
			      { distributedOnAgentExpression = theExpression; }



  public DataExpression getTcmTaskTreeParentExpression()
				       { return tcmTaskTreeParentExpression; }
  public void           setTcmTaskTreeParentExpression(
						DataExpression theExpression )
			      { tcmTaskTreeParentExpression = theExpression; }


  public DataExpression getTdlRefInExpression() { return tdlRefInExpression; }
  public void           setTdlRefInExpression( DataExpression theExpression )
				       { tdlRefInExpression = theExpression; }


  public DataExpression getTcmTaskTreeNameExpression()
				 { return tcmTaskTreeNameExpression; }
  public void           setTcmTaskTreeNameExpression(
						 DataExpression theExpression )
				 { tcmTaskTreeNameExpression = theExpression; }




  public String  getCxxExternalSubtaskName() { return cxxExternalSubtaskName; }
  public void    setCxxExternalSubtaskName( String theCxxExternalSubtaskName )
			{ cxxExternalSubtaskName = theCxxExternalSubtaskName; }


  public boolean       hasChildStatement(){return getChildStatement() != null;}
  public DataStatement getChildStatement()
  {
    switch ( getConstraintType() )
    {
	/* Note:  Do not report exception-handler children statements.
	 * Exception handlers have no TCM tree-node reference as currently
	 * specified by the TCM interface.
	 */

      case DataConstraint.ON_TERMINATE:
	return getOnTerminateTask();

      default:
	return null;
    }
  }


	/* Allow searching of our expressions... */
  public boolean hasSubcomponentFraction ( String theString )
  {
    if ( getHasEventTagTaskIndexes() )
    {
      for ( int i=0;  i < getEventTagTaskIndexes() . length;  i++ )
      {
	if (   (   (    getEventTagTaskIndexes() [ i ] instanceof DataComponent
		     == true )
		&& ( ((DataComponent) (getEventTagTaskIndexes() [ i ]))
		        . hasSubcomponentFraction ( theString ) ) )
	    || (   (    getEventTagTaskIndexes() [ i ] instanceof DataComponent
		     == false )
		&& ( getEventTagTaskIndexes() [ i ]
		       . toString() . indexOf ( theString ) != -1 ) ) )
	{
	  return true;
	}
      }
    }

    return super                      . hasSubcomponentFraction(theString)
      || (   ( getTimeExpression()    != null )
	  && ( getTimeExpression()    . hasSubcomponentFraction(theString) ) )
      || (   ( getNumericExpression() != null )
	  && ( getNumericExpression() . hasSubcomponentFraction(theString) ) );
  }


	/* Allow searching of our expressions... */
  public void runOnSubcomponentFraction (
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
    super . runOnSubcomponentFraction ( theString,
					theRunOnSubcomponentObject,
					theArgumentObject );

    if ( getHasEventTagTaskIndexes() )
    {
      for ( int i=0;  i < getEventTagTaskIndexes() . length;  i++ )
      {
	if ( getEventTagTaskIndexes() [ i ] instanceof DataComponent )
	  ((DataComponent) (getEventTagTaskIndexes() [ i ]))
	     . runOnSubcomponentFraction ( theString,
					   theRunOnSubcomponentObject,
					   theArgumentObject );
	else
	  DataComponent.staticRunOnStringFraction (
				  getEventTagTaskIndexes() [ i ] . toString(),
				  theString,
				  theRunOnSubcomponentObject,
				  theArgumentObject );
      }
    }


    if ( getTimeExpression() != null )
      getTimeExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getNumericExpression() != null )
      getNumericExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getExceptionHandlerTask() != null )
      getExceptionHandlerTask()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );


	/* Skip on-terminate Task/constraint.  The On-terminate task is
	 * considered a child, and should not be searched twice.
	 */

    if ( getDistributedFormatStringExpression() != null )
      getDistributedFormatStringExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getDistributedOnAgentExpression() != null )
      getDistributedOnAgentExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getTcmTaskTreeParentExpression() != null )
      getTcmTaskTreeParentExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getTdlRefInExpression() != null )
      getTdlRefInExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );

    if ( getTcmTaskTreeNameExpression() != null )
      getTcmTaskTreeNameExpression()
	. runOnSubcomponentFraction ( theString,
				      theRunOnSubcomponentObject,
				      theArgumentObject );
  }


  	/* Validates code that is outside of any Task */
  public void validateExternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
    if ( getHasEventTagTaskOfThis() )
    {
      theReturnValue
	. addWarning ( this )
	. write ( "Using " )
	. write ( DataComponent.CXX_TCM_ROOT_NODE )
	. write ( " for \"THIS\"\n" );

      if (   ( getConstraintType() == DataConstraint.SEQUENTIAL_HANDLING )
	  || ( getEventConstraintOption() == DataConstraint.HANDLING     )
	  || (    getHasEventTagTask()
	       && ( getHasEventConstraintOption() == false )             ) )
      {
	theReturnValue
	  . addWarning ( this )
	  . write ( DataComponent.CXX_TCM_ROOT_NODE )
	  . write ( " [\"THIS\"] Handling never runs.\n" );
      }
    }

    if ( getHasEventTagTaskOfPrevious() )
    {
      theReturnValue
	. addWarning ( this )
	. write ( "Using " )
	. write ( DataComponent.CXX_TCM_PREVIOUS_EXTERNAL_TASK_REF )
	. write ( " for \"PREVIOUS\"\n" );
    }

    if ( getConstraintType() == DataConstraint.ON_AGENT )
    {
      theReturnValue
	. addError ( this )
	. write ( "ON-AGENT constraint may ONLY be used inside a Task-body." )
	. write ( "  It may NOT not be used inside a function or method!\n"  );
    }

    validateInternalCode ( theReference, theReturnValue, false /*IsInternal*/);
  }


	/* Validates code that is inside of a Task */
  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue)
    throws CompilationException
  {
    validateInternalCode ( theReference, theReturnValue, true );
  }

  public void validateInternalCode( int                         theReference,
				    DataValidateCodeReturnValue theReturnValue,
				    boolean                     theIsInternal )
    throws CompilationException
  {
    DataComponent       ancestor, tagTaskDataComponent;
    DataTaskDefinition  ancestorDataTaskDefinition;
    int                 iterationCount;

    if ( getHasNonStandardEventTagTask() )
    {
    	/* Find our DataTaskDefinition Ancestor */
      for ( ancestor  = this;
	    ancestor != null;
	    ancestor  = ancestor . getParent()
	   )
      {
	if ( ancestor instanceof DataTaskDefinition )
	  break;
      }
      if ( ancestor == null )
      {
	throw new CompilationException (
	   getMessageFilenameLead() + getLineNumberString()
	 + ": Programmer Error:  Unable to find DataTaskDefinition ancestor.");
      }
      ancestorDataTaskDefinition = (DataTaskDefinition) ancestor;

	/* Find the tagTaskDataComponent */
      tagTaskDataComponent = ancestorDataTaskDefinition
			      . getDataComponentWithName ( getEventTagTask() );
	/* If this is null, the error will be picked up elsewhere. */
      if ( tagTaskDataComponent != null )
      {
	  /* Find out how many iteration loops enclose the tagTask Object */
	  /* Note:  This can throw a CompilationException.                */
	iterationCount = ancestorDataTaskDefinition
			   . getIterationParentCount ( tagTaskDataComponent );


	  /* And lets double-check this stuff. */
	if ( tagTaskDataComponent instanceof DataBindTaskStatement )
	{
	  if ( getHasEventTagTaskIndexes() == true )
	  {
	    theReturnValue
	      . addError ( this )
	      . write ( "Too many Iteration-Indexes for TDL_BIND " )
	      . write ( "Constraint Reference.  (There is (are) "  )
	      . write ( "" + getEventTagTaskIndexes() . length     )
	      . write ( " array index(es) specified.  "            )
	      . write ( "There should be zero.)\n"                 );
	  }
	}
	else
	{
	  if (   ( getHasEventTagTaskIndexes()       == true           )
	      && ( getEventTagTaskIndexes() . length >  iterationCount ) )
	  {
	    theReturnValue
	      . addError ( this )
	      . write ( "Too many Iteration-Indexes for Constraint" )
	      . write ( " Reference.  (Reference is enclosed in "   )
	      . write ( "" + iterationCount                         )
	      . write ( " iteration loop(s) and there is (are) "    )
	      . write ( "" + getEventTagTaskIndexes() . length      )
	      . write ( " array index(es) specified.)\n"            );
	  }

	  if (  (     iterationCount                    >  0                )
	      &&(   ( getEventTagTaskIndexes()          == null           )
		 || ( getEventTagTaskIndexes() . length <  iterationCount ) ) )
	  {
	    theReturnValue
	      . addWarning ( this )
	      . write ( "Using an Iteration-Set of References for "         )
	      . write ( "Constraint Reference.  (Reference is enclosed in " )
	      . write ( "" + iterationCount                                 )
	      . write ( " iteration loop(s) and there is (are) "            )
	      . write (   ( getEventTagTaskIndexes() == null )
			? "0" : ("" + getEventTagTaskIndexes() . length)    )
	      . write ( " array index(es) specified.)\n"                    );
	  }
	} /* IF (tagTaskDataComponent instanceof DataBindTaskStatement) ELSE */
      } /* if ( tagTaskDataComponent != null ) */
    } /* if ( getHasNonStandardEventTagTask() ) */



    for ( int i=0;
	  getHasEventTagTaskIndexes() && (i < getEventTagTaskIndexes().length);
	  i++ )
    {
      if (   ( getEventTagTaskIndexes()[i] . toString() . trim() . charAt ( 0 )
	       == '0' )
	  && ( getEventTagTaskIndexes()[i] . toString() . trim() . length()
	       >   1  ) )
      {
	theReturnValue
	  . addWarning ( this )
	  . write ( "Task Index is specified in octal.\n" );
	break; /* Don't test any more indexes... */
      }
    }

    if (   (   ( getConstraintType() == DataConstraint.SEQUENTIAL_HANDLING  )
	    || ( getConstraintType() == DataConstraint.SEQUENTIAL_EXPANSION )
	    || ( getConstraintType() == DataConstraint.SEQUENTIAL_EXECUTION ) )
	&& (   getHasEventTagTaskOfChild()
	    || getHasEventTagTaskOfThis() )
		/* Actually, SEQUENTIAL HANDLING THIS is legal.. */
	&& (   ( getConstraintType() != DataConstraint.SEQUENTIAL_HANDLING  )
	    || ( getHasEventTagTaskOfThis() == false                        ) )
	)
    {
      theReturnValue
	. addError ( this )
	. write ( "Circular dependency detected.  Sequential on " );

      if ( getHasEventTagTaskOfChild() )
	theReturnValue . getDataDestination() . write ( "CHILD.\n" );
      else
	theReturnValue . getDataDestination() . write ( "THIS.\n" );
    }


    if (   ( getConstraintType() == DataConstraint.SERIAL )
	&& (    getHasEventTagTaskOfChild()
	     || getHasEventTagTaskOfThis() ) )
    {
      theReturnValue
	. addError ( this )
	. write ( "Circular dependency detected.  Serial on " );

      if ( getHasEventTagTaskOfChild() )
	theReturnValue . getDataDestination() . write ( "CHILD.\n" );
      else
	theReturnValue . getDataDestination() . write ( "THIS.\n" );
    }



    if (   (     getConstraintType() == DataConstraint.DISABLE_UNTIL_EVENT  )
	|| (   ( getConstraintType() == DataConstraint.DISABLE_FOR_TIME   )
	    && ( getHasEventTagTask()                                     ) ) )
    {
      if (    getHasEventTagTaskOfChild()
	   &&
		    /* Constraining child handling until child {HPA-} {EAC} */
	   (   (   ( getHasConstraintOption() == false                   )
		|| ( getConstraintOption()    == DataConstraint.HANDLING )
		)
		    /*    Constraining child planning until child {HP-} {EAC}*/
		    /* Or Constraining child planning until child {A}    {AC}*/
	    || (   ( getConstraintOption()         == DataConstraint.EXPANSION)
	        && (   (getEventConstraintOption() != DataConstraint.EXECUTION)
		    || (getStateBoundary()         != DataConstraint.ENABLED  )
		    )
	        )
		    /*   Constraining child achievement until child {A} {EAC}*/
		    /*Or Constraining child achievement until child {-} {C}  */
	    || (   ( getConstraintOption()         == DataConstraint.EXECUTION)
	        && (   (getEventConstraintOption() == DataConstraint.EXECUTION)
		    || (   ( getHasEventConstraintOption() == false         )
			&& ( getStateBoundary() == DataConstraint.COMPLETED ) )
		    )
		)
	    )
	  )
      {
	theReturnValue
	  . addError ( this )
	  . write ( "Circular dependency detected.  Disable on CHILD.\n" );
      }


      if (    getHasEventTagTaskOfThis()
	   && 
	    /* Constraining child until THIS {PA} {C} */
	   (   (   ( getEventConstraintOption() == DataConstraint.EXPANSION )
		|| ( getEventConstraintOption() == DataConstraint.EXECUTION ) )
	    && ( getStateBoundary() == DataConstraint.COMPLETED ) )
	  )
      {
	theReturnValue
	  . addError ( this )
	  . write ( "Circular dependency detected.  Disable on THIS.\n" );
      }

    } /* IF ( DISABLE w/ event ) */




    if (   ( getConstraintType() == DataConstraint.TERMINATE_AT_EVENT )
	&& getHasEventTagTaskOfChild() )
    {
      theReturnValue
	. addWarning ( this )
	. write ( "Are you certain you want to Terminate on CHILD?\n" );
    }


    if (   (   ( getConstraintType() == DataConstraint.TERMINATE_AT_EVENT )
	    || ( getConstraintType() == DataConstraint.TERMINATE_IN_TIME  ) )
	&& getHasEventTagTaskOfThis() )
    {
	theReturnValue
	  . addWarning ( this )
	  . write ( "Are you certain you want to Terminate on THIS?\n" );
    }

    if (   ( getConstraintType() == DataConstraint.ACTIVATE_AT_EVENT )
	&& getHasEventTagTaskOfChild() )
    {
	theReturnValue
	  . addWarning ( this )
	  . write ( "Are you certain you want to Activate on CHILD?\n" );
    }

    if (   (   ( getConstraintType() == DataConstraint.ACTIVATE_AT_EVENT )
	    || ( getConstraintType() == DataConstraint.ACTIVATE_IN_TIME  ) )
	&& getHasEventTagTaskOfThis() )
    {
	theReturnValue
	  . addWarning ( this )
	  . write ( "Are you certain you want to Activate on THIS?\n" );
    }


    if (   ( getConstraintType()    == DataConstraint.MAXIMUM_ACTIVATE )
	&& ( getNumericExpression() == null ) )
    {
      theReturnValue
	. addError ( this )
	. write ( "MAXIMUM ACTIVATE specification incomplete.\n" );
    }

    if (   ( getConstraintType()    == DataConstraint.MAXIMUM_TRIGGER )
	&& ( getNumericExpression() == null ) )
    {
      theReturnValue
	. addError ( this )
	. write ( "MAXIMUM ACTIVATE specification incomplete.\n" );
    }

    if (   ( getConstraintType() == DataConstraint.EXCEPTION_HANDLER )
	&& (   ( getExceptionHandlerTask()                            == null )
	    || ( getExceptionHandlerTask() . getTaskName()            == null )
	    || ( getExceptionHandlerTask() . getTaskName() . length() <= 0    )
	    )
        )
    {
      theReturnValue
	. addError ( this )
	. write ( "EXCEPTION HANDLER specification incomplete.\n" );
    }

    if (   ( getConstraintType() == DataConstraint.ON_TERMINATE )
	&& (   ( getOnTerminateTask()                            == null )
	    || ( getOnTerminateTask() . getTaskName()            == null )
	    || ( getOnTerminateTask() . getTaskName() . length() <= 0    )
	    )
        )
    {
      theReturnValue
	. addError ( this )
	. write ( "ON TERMINATE task specification incomplete.\n" );
    }

    if (   ( theIsInternal       == true                      )
	&& ( getConstraintType() == DataConstraint.TDL_REF_IN ) )
    {
      theReturnValue
	. addError ( this )
	. write ( "The \"TDL_REF IN\" constraint may not be used inside a " )
	. write ( "Task.  It may only be employed outside of any Task, "    )
	. write ( "on SPAWNs located inside C++ functions or methods.\n"    );
    }
  } /* public void validateInternalCode ( ... ) */



  public boolean isValid ( int theObjectSubsetToValidate )
  {
    if (   ( getConstraintType() < 0 )
	|| ( getConstraintType() > DataConstraint.MAX_CONSTRAINT_INDEX ) )
      return false;

    switch ( getConstraintType() )
    {
      case DataConstraint.DISABLE_UNTIL_EVENT:
      case DataConstraint.TERMINATE_AT_EVENT:
      case DataConstraint.ACTIVATE_AT_EVENT:

	if (   ( getStateBoundary() < 0 )
	    || ( getStateBoundary() > DataConstraint.MAX_STATE_BOUNDARY_INDEX )
	    )
	  return false;

	if ( getEventTagTask() == null )
	  return false;

	break;


      case DataConstraint.DISABLE_UNTIL_TIME:
      case DataConstraint.TERMINATE_AT_TIME:
      case DataConstraint.ACTIVATE_AT_TIME:

	if (   (   ( getHasHours()   == false )
		|| ( getHasMinutes() == false ) )
	    && ( getTimeExpression() == null    ) )
	  return false;

	break;


      case DataConstraint.DISABLE_FOR_TIME:
      case DataConstraint.TERMINATE_IN_TIME:
      case DataConstraint.ACTIVATE_IN_TIME:

	if (   ( getHasSeconds()     == false )
	    && ( getTimeExpression() == null  ) )
	  return false;

	break;


      case DataConstraint.MAXIMUM_ACTIVATE:
      case DataConstraint.MAXIMUM_TRIGGER:

	if ( getNumericExpression() == null )
	  return false;

	break;


      case DataConstraint.EXCEPTION_HANDLER:

	if (   ( getExceptionHandlerTask()                            == null )
	    || ( getExceptionHandlerTask() . getTaskName()            == null )
	    || ( getExceptionHandlerTask() . getTaskName() . length() <= 0    )
	    )
	  return false;

	break;


      case DataConstraint.ON_TERMINATE:

	if (   ( getOnTerminateTask()                            == null )
	    || ( getOnTerminateTask() . getTaskName()            == null )
	    || ( getOnTerminateTask() . getTaskName() . length() <= 0    )
	    )
	  return false;

	break;


      case DataConstraint.DISTRIBUTED_FORMAT:

	if ( getDistributedFormatStringExpression() == null )
	  return false;

	if (   getDistributedFormatStringExpression() . getSubcomponentsCount()
	    <= 0 )
	  return false;

	break;


      case DataConstraint.ON_AGENT:
	if ( getDistributedOnAgentExpression() == null )
	  return false;

	if (   getDistributedOnAgentExpression() . getSubcomponentsCount()
	    <= 0 )
	  return false;

	break;


      case DataConstraint.TCM_TASK_TREE_PARENT:
	if ( getTcmTaskTreeParentExpression() == null )
	  return false;

	if (   getTcmTaskTreeParentExpression() . getSubcomponentsCount()
	    <= 0 )
	  return false;

	break;


      case DataConstraint.TDL_REF_IN:
	if ( getTdlRefInExpression() == null )
	  return false;

	if (   getTdlRefInExpression() . getSubcomponentsCount()
	    <= 0 )
	  return false;

	break;


      case DataConstraint.TCM_TASK_TREE_NAME:
	if ( getTcmTaskTreeNameExpression() == null )
	  return false;

	if (   getTcmTaskTreeNameExpression() . getSubcomponentsCount()
	    <= 0 )
	  return false;

	break;
    }

    return true;
  }



  public void generate ( DataDestination  theOutputDestination,
			 int              theObjectSubsetToGenerate )
  {
       	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToGenerate, "generate" );

	/* If we are doing CXX generation... */
    if ( isCxxSubset ( theObjectSubsetToGenerate ) )
      generateCxxTaskInternal ( theOutputDestination );


    int      eventTagTaskIndex = 0;
    int      keywordIndex        = 0;
    String[] keywordStrings      = getConstraintTypeTokens();

	/* Error check that... */
    if ( (keywordStrings == null) || (keywordStrings.length <= 0) )
    {
      System.err.println ( "[DataConstraint:generate]  Error:  "
			   + "No keywordStrings!!! ( " + keywordStrings + ")");
      return;
    }

	/* Initialize us to generate non-significant tokens... */
    initializeGenerateSubcomponentIndex();

	/* Write any pre-first-token non-significant tokens */
    generateSubcomponents ( DataComponent.FIRST_TOKEN_INDEX,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );

	/* Write any pre-first-keyword non-significant tokens */
    generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    getIndex( DataComponent.FIRST_TOKEN_INDEX ) == 0 );

	/* Write the first keyword... */
    theOutputDestination . write ( keywordStrings [ keywordIndex ] );


	/* Write this rest of this constraint... */
    switch ( getConstraintType() )
    {
		/* Simple constraints */
	case DataConstraint.PARALLEL:
	case DataConstraint.WAIT:
        case DataConstraint.TERMINATE:
        case DataConstraint.ACTIVATE:
		/* Nothing left to do... */
	  break;


		/* Constraints with second token */
	case DataConstraint.EXPAND_FIRST:
	case DataConstraint.DELAY_EXPANSION:
    		/* Write non-significant tokens & next keyword */
	  keywordIndex++;
	  generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				  theOutputDestination,
				  theObjectSubsetToGenerate, false );
	  theOutputDestination . write ( keywordStrings [ keywordIndex ] );
	  break;



		/* Fancy constraints */


		/* Constraints with optional event-tag */
	case DataConstraint.SEQUENTIAL_HANDLING:
	case DataConstraint.SEQUENTIAL_EXPANSION:
	case DataConstraint.SEQUENTIAL_EXECUTION:
    		/* Write non-significant tokens & next keyword */
	  keywordIndex++;
	  generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				  theOutputDestination,
				  theObjectSubsetToGenerate, false );
	  theOutputDestination . write ( keywordStrings [ keywordIndex ] );

	    /* ***** !!!!!  NO BREAK !!!!! ***** */

	case DataConstraint.SERIAL:

		/* Write optional event-tag */
	  if ( getHasEventTagTask() )
	  {
		/* Write non-signif. tokens & generate the event-tag */
	    generateEventTagTask ( theOutputDestination,
				   theObjectSubsetToGenerate );
	  }
	  break;




	case DataConstraint.DISABLE_UNTIL_EVENT:
    		/* Write non-significant tokens & constraint-option */
	  generateSubcomponents ( DataConstraint.CONSTRAINT_OPTION,
				  theOutputDestination,
				  theObjectSubsetToGenerate, false );
	  if ( getHasConstraintOption() )
	  {
	    theOutputDestination . write ( getConstraintOptionString() );
	  }

	    /* ***** !!!!!  NO BREAK !!!!! ***** */

	case DataConstraint.TERMINATE_AT_EVENT:
	case DataConstraint.ACTIVATE_AT_EVENT:

    		/* Write non-significant tokens & next keyword */
	  keywordIndex++;
	  generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				  theOutputDestination,
				  theObjectSubsetToGenerate, false );
	  theOutputDestination . write ( keywordStrings [ keywordIndex ] );
	  
		/* Generate the event */
	  generateEvent ( theOutputDestination, theObjectSubsetToGenerate );
	  break;




	case DataConstraint.DISABLE_UNTIL_TIME:
	case DataConstraint.DISABLE_FOR_TIME:

    		/* Write non-significant tokens & constraint-option */
	  generateSubcomponents ( DataConstraint.CONSTRAINT_OPTION,
				  theOutputDestination,
				  theObjectSubsetToGenerate, false );
	  if ( getHasConstraintOption() )
	  {
	    theOutputDestination . write ( getConstraintOptionString() );
	  }

	    /* ***** !!!!!  NO BREAK !!!!! ***** */

        case DataConstraint.TERMINATE_AT_TIME:
        case DataConstraint.TERMINATE_IN_TIME:
        case DataConstraint.ACTIVATE_AT_TIME:
        case DataConstraint.ACTIVATE_IN_TIME:

    		/* Write non-significant tokens & next keyword */
	  keywordIndex++;
	  generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				  theOutputDestination,
				  theObjectSubsetToGenerate, false );
	  theOutputDestination . write ( keywordStrings [ keywordIndex ] );


	    /* ***** !!!!!  NO BREAK !!!!! ***** */

        case DataConstraint.MONITOR_PERIOD:

	  generateTime ( theOutputDestination, theObjectSubsetToGenerate );



	  if (   ( getConstraintType() == DataConstraint.DISABLE_FOR_TIME  )
	      || ( getConstraintType() == DataConstraint.TERMINATE_IN_TIME )
	      || ( getConstraintType() == DataConstraint.ACTIVATE_IN_TIME  ) )
	  {
    		/* Write remaining non-significant tokens */
	    keywordIndex++;
	    generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				    theOutputDestination,
				    theObjectSubsetToGenerate, false );

	    if ( getHasEventTagTask() )
	    {
		/* Write remaining "AFTER" keyword (token) */
	      theOutputDestination . write ( keywordStrings [ keywordIndex ] );

		/* Generate the "AFTER" event */
	      generateEvent ( theOutputDestination, theObjectSubsetToGenerate);
	    }
	  }

	  break;



        case DataConstraint.MAXIMUM_ACTIVATE:
        case DataConstraint.MAXIMUM_TRIGGER:
	    		/* Write non-significant tokens & next keyword */
	  keywordIndex++;
	  generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				  theOutputDestination,
				  theObjectSubsetToGenerate, true );
	  theOutputDestination . write ( keywordStrings [ keywordIndex ] );

	   /* If we are missing leadings whitespace, correct the problem.
	    * (Otherwise the tokens will abut, and form an incorrect token.) */
	  if (    getNumericExpression() . hasLeadingWhitespaceSubcomponent()
	       == false )
	  {
	    theOutputDestination . write ( DataComponent.SPACE_STRING );
	  }

		/* Write the (numeric-string) number */
	  getNumericExpression() . generate ( theOutputDestination,
					      DataComponent.ENTIRE_OBJECT );
	  break;



        case DataConstraint.EXCEPTION_HANDLER:
	    	/* Write non-significant tokens & next keyword */
	  keywordIndex++;
	  generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				  theOutputDestination,
				  theObjectSubsetToGenerate, true );
	  theOutputDestination . write ( keywordStrings [ keywordIndex ] );

		/* Write the exception task */
	  getExceptionHandlerTask() . generate ( theOutputDestination,
						 DataSpawnTask.TASK_ONLY );
	  break;



        case DataConstraint.ON_TERMINATE:
	    	/* Write non-significant tokens & next keyword */
	  keywordIndex++;
	  generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				  theOutputDestination,
				  theObjectSubsetToGenerate, true );
	  theOutputDestination . write ( keywordStrings [ keywordIndex ] );

		/* Generate task labels */
	  getOnTerminateTask()
	    . generateLabels ( theOutputDestination,
			       DataComponent.ENTIRE_OBJECT,
			       null, /* Irrelevant without #line macros */
			       true  /* Disable #line macros            */ );

	  if ( getHasOnTerminateSpawnKeyword() )
	  {
    		/* Write remaining non-significant tokens */
	    keywordIndex++;
	    generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				    theOutputDestination,
				    theObjectSubsetToGenerate, false );

		/* Write remaining "SPAWN" keyword (token) */
	    theOutputDestination . write ( keywordStrings [ keywordIndex ] );
	  }
		/* Write the on-terminate task */
	  getOnTerminateTask() . generate ( theOutputDestination,
					    DataSpawnTask.TASK_ONLY );
	  break;



      case DataConstraint.DISTRIBUTED_FORMAT:
		/* Write format string-expression */
	if ( getDistributedFormatStringExpression() != null )
	  getDistributedFormatStringExpression()
	    . generate ( theOutputDestination, theObjectSubsetToGenerate );

	break;



      case DataConstraint.ON_AGENT:
		/* Generate "AGENT" DataExpression */
	if ( getDistributedOnAgentExpression() != null )
	  getDistributedOnAgentExpression()
	    . generate ( theOutputDestination, theObjectSubsetToGenerate );
	break;


      case DataConstraint.TCM_TASK_TREE_PARENT:
		/* Generate "PARENT" DataExpression */
	if ( getTcmTaskTreeParentExpression() != null )
	  getTcmTaskTreeParentExpression()
	    . generate ( theOutputDestination, theObjectSubsetToGenerate );
	break;


      case DataConstraint.TDL_REF_IN:
	    	/* Write non-significant tokens & next keyword */
	keywordIndex++;
	generateSubcomponents ( DataConstraint.KEYWORD + keywordIndex,
				theOutputDestination,
				theObjectSubsetToGenerate, true );
	theOutputDestination . write ( keywordStrings [ keywordIndex ] );

		/* Generate "TDL_REF_IN" DataExpression */
	if ( getTdlRefInExpression() != null )
	  getTdlRefInExpression()
	    . generate ( theOutputDestination, theObjectSubsetToGenerate );
	break;


      case DataConstraint.TCM_TASK_TREE_NAME:
		/* Generate "NAME" DataExpression */
	if ( getTcmTaskTreeNameExpression() != null )
	  getTcmTaskTreeNameExpression()
	    . generate ( theOutputDestination, theObjectSubsetToGenerate );
	break;


    } /* switch ( getConstraintType() ) */


	/* Write any remaining non-significant tokens */
    generateAllRemainingSubcomponents ( theOutputDestination,
					theObjectSubsetToGenerate, false );
  }


  protected void generateEventTagTask (
				   DataDestination  theOutputDestination,
				   int              theObjectSubsetToGenerate )
  {
    Object[] eventTagTaskIndexes = getEventTagTaskIndexes();

	/* Idiocy check */
    if ( getHasEventTagTask() == false )
    {
      System.err.println ( "[DataConstraint:generateEventTagTask]  Error:  "
			   + "No Event-Tag-Task available to generate..." );
      return;
    }

	/* Write non-significant tokens & event-tag */
    generateSubcomponents ( DataConstraint.TAG_TASK,
			    theOutputDestination,
			    theObjectSubsetToGenerate, true );
    theOutputDestination . write ( getEventTagTask() );


	/* If we do have indexes... */
    if ( getHasEventTagTaskIndexes() )
    {
      for ( int i=0;   i < eventTagTaskIndexes.length;   i++ )
      {
	  /* Write non-significant tokens & open-index */
	generateSubcomponents ( DataConstraint.BEGIN_TAG_TASK_INDEX + i,
				theOutputDestination,
				theObjectSubsetToGenerate, false );
	theOutputDestination . write ( DataConstraint.BEGIN_TAG_TASK_INDEX );


	  /* Write non-significant tokens & index */      
	generateSubcomponents ( "" + i,
				theOutputDestination,
				theObjectSubsetToGenerate, false );
	if ( eventTagTaskIndexes [ i ] != null )
	{
	  if ( eventTagTaskIndexes [ i ] instanceof DataComponent )
	  {
	    ((DataComponent) (eventTagTaskIndexes [ i ]))
	      . generate ( theOutputDestination, DataComponent.ENTIRE_OBJECT );
	  }
	  else
	  {
	    theOutputDestination
	      . write ( eventTagTaskIndexes [ i ] . toString() );
	  }
	}

	  /* Write non-significant tokens & close-index */
	generateSubcomponents ( DataConstraint.END_TAG_TASK_INDEX  + i,
				theOutputDestination,
				theObjectSubsetToGenerate, false );
	theOutputDestination . write ( DataConstraint.END_TAG_TASK_INDEX );
      }
    }
  }


  protected void generateEvent ( DataDestination  theOutputDestination,
				 int              theObjectSubsetToGenerate )
  {
    generateEventTagTask ( theOutputDestination, theObjectSubsetToGenerate );

	/* Write non-signif. tokens & event-constraint-option */
    generateSubcomponents ( DataConstraint.EVENT_CONSTRAINT_OPTION,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );
    if ( getHasEventConstraintOption() )
    {
      theOutputDestination . write ( getEventConstraintOptionString() );
    }

	/* Write non-signif. tokens & State-Boundary */
    generateSubcomponents ( DataConstraint.STATE_BOUNDARY,
			    theOutputDestination,
			    theObjectSubsetToGenerate, false );
    theOutputDestination . write ( getStateBoundaryString() );
  }


  protected void generateTime ( DataDestination  theOutputDestination,
				int              theObjectSubsetToGenerate )
  {
    int  indent;

    if ( getTimeExpression() != null )
    {
	/* Write the time-expression, properly indenting it... */
      indent = theOutputDestination . indentToCurrentColumn ( );

	/* Remove indentation for "    // " string */
      if ( indent >= theOutputDestination . getNewlineText() . length() )
      {
	theOutputDestination
	  . removeIndent( theOutputDestination . getNewlineText() . length() );
	indent -= theOutputDestination . getNewlineText() . length();
      }

      getTimeExpression() . generate ( theOutputDestination,
				       DataComponent.ENTIRE_OBJECT );
      theOutputDestination . removeIndent ( indent );
    }
    else
    {
	/* Write non-significant tokens & hours */
      generateSubcomponents ( DataConstraint.PRE_TIME_HOURS,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );
      if ( getHasHours() )
      {
	theOutputDestination . write ( getHours() );
      }
      generateSubcomponents ( DataConstraint.POST_TIME_HOURS,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

	/* Print hours-minutes separator? */
      if ( getHasHours() )
      {
	theOutputDestination . write ( DataConstraint.TIME_SEPARATOR );
      }

	/* Write non-significant tokens & minutes */
      generateSubcomponents ( DataConstraint.PRE_TIME_MINUTES,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );
      if ( getHasMinutes() )
      {
	theOutputDestination . write ( getMinutes() );
      }
      else if ( getHasHours() )  /* Handle problem case */
      {
	theOutputDestination . write ( "0" );
      }

      generateSubcomponents ( DataConstraint.POST_TIME_MINUTES,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

	/* Print minutes-seconds separator? */
      if ( getHasMinutes() || getHasHours() )
      {
	theOutputDestination . write ( DataConstraint.TIME_SEPARATOR );
      }

	/* Write non-significant tokens & seconds */
      generateSubcomponents ( DataConstraint.PRE_TIME_SECONDS,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );
      if ( getHasSeconds() )
      {
	theOutputDestination . write ( getSeconds() );
      }
      else if ( getHasHours() || getHasMinutes() )/* Handle problem case */
      {
	theOutputDestination . write ( "0" );
      }

      generateSubcomponents ( DataConstraint.POST_TIME_SECONDS,
			      theOutputDestination,
			      theObjectSubsetToGenerate, false );

      /* Only print seconds-fractions separator if we have fractions */
      if ( getHasFractionsOfASecond() )
      {
	theOutputDestination
	  . write ( DataConstraint.FRACTIONAL_TIME_SEPARATOR );
      }

	/* Write non-significant tokens & fractions of a second */
      generateSubcomponents( DataConstraint.PRE_TIME_FRACTIONS_OF_A_SECOND,
			     theOutputDestination,
			     theObjectSubsetToGenerate, false );
      if ( getHasFractionsOfASecond() )
      {
	theOutputDestination . write ( getFractionsOfASecond() );
      }
    }
  }




    /** Note:  This method is presumed to be called from DataSpawnTask's
      * generateCxx, and it assumes that it is generating constraints
      * as elements of a Gigantic set of method invocations.
      */
  public void generateCxxTaskInternal ( DataDestination  theOutputDestination )
  {
	/* Indent everything to this column ... */
    int   indent = theOutputDestination . indentToCurrentColumn();


	/* Find our Cxx Function */
    switch ( getConstraintType() )
    {
      case DataConstraint.EXPAND_FIRST:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_EXPAND_FIRST_CLASS );
	break;

      case DataConstraint.DELAY_EXPANSION:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_DELAY_EXPANSION_CLASS );
	break;

      case DataConstraint.PARALLEL:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_PARALLEL_CLASS );
	break;

      case DataConstraint.WAIT:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_WAIT_CLASS );
	break;

      case DataConstraint.SEQUENTIAL_HANDLING:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_SEQUENTIAL_HANDLING_CLASS );
	break;

      case DataConstraint.SEQUENTIAL_EXPANSION:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_SEQUENTIAL_EXPANSION_CLASS );
	break;

      case DataConstraint.SEQUENTIAL_EXECUTION:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_SEQUENTIAL_EXECUTION_CLASS );
	break;

      case DataConstraint.SERIAL:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_SERIAL_CLASS );
	break;

      case DataConstraint.DISABLE_UNTIL_EVENT:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_DISABLE_UNTIL_EVENT_CLASS );
	break;

      case DataConstraint.DISABLE_UNTIL_TIME:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_DISABLE_UNTIL_CLASS );
	break;

      case DataConstraint.DISABLE_FOR_TIME:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_DISABLE_FOR_CLASS );
	break;

      case DataConstraint.TERMINATE_AT_EVENT:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_TERMINATE_AT_EVENT_CLASS );
	break;

      case DataConstraint.TERMINATE_AT_TIME:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_TERMINATE_AT_CLASS );
	break;

      case DataConstraint.TERMINATE_IN_TIME:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_TERMINATE_IN_CLASS );
	break;

      case DataConstraint.TERMINATE:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_TERMINATE_IMMEDIATE_CLASS );
	break;

      case DataConstraint.ACTIVATE_AT_EVENT:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_ACTIVATE_AT_EVENT_CLASS );
	break;

      case DataConstraint.ACTIVATE_AT_TIME:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_ACTIVATE_AT_CLASS );
	break;

      case DataConstraint.ACTIVATE_IN_TIME:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_ACTIVATE_IN_CLASS );
	break;

      case DataConstraint.ACTIVATE:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_ACTIVATE_IMMEDIATE_CLASS );
	break;

      case DataConstraint.MAXIMUM_ACTIVATE:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_SET_MAXIMUM_ACTIVATE_CLASS );
	break;

      case DataConstraint.MAXIMUM_TRIGGER:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_SET_MAXIMUM_TRIGGER_CLASS );
	break;

      case DataConstraint.MONITOR_PERIOD:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_SET_MONITOR_PERIOD_CLASS );
	break;

      case DataConstraint.EXCEPTION_HANDLER:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_ADD_EXCEPTION_HANDLER_CLASS );
	break;

      case DataConstraint.ON_TERMINATE:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_ON_TERMINATE_CLASS );
	break;


      case DataConstraint.ON_AGENT:
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_ON_AGENT_CLASS );
	break;


	/***************/
	/* ABORT CASES */
	/***************/
      default:
	String  errorString = null;
	switch ( getConstraintType() )
	{
	  case DataConstraint.DISTRIBUTED_FORMAT:
	    errorString = "Distributed FORMAT";
	    break;
	  case DataConstraint.TCM_TASK_TREE_PARENT:
	    errorString = "PARENT";
	    break;
	  case DataConstraint.TDL_REF_IN:
	    errorString = "TDL_REF IN";
	    break;
	  case DataConstraint.TCM_TASK_TREE_NAME:
	    errorString = "NAME";
	    break;
	}
	if ( errorString != null )
	{
	  System.err.println ( "[DataConstraint:generateCxxTaskInternal]  "
			       + "Error:  Attempting to generate a "
			       + errorString
			       + " constraint.  This should not be "
			       + "happening.  Aborting this constraint..." );
	}
	else
	{
	  System.err.println ( "[DataConstraint:generateCxxTaskInternal]  "
			       + "Error:  Unknown Constraint-Type ("
			       + getConstraintType()
			       + ")  Aborting this constraint..." );
	}

	    /* Stop indenting everything... */
	theOutputDestination . removeIndent ( indent );

	    /* ABORT */
	return;

    } /* switch ( getConstraintType() )  // Finding our Cxx Function */


    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );
    indent += DataComponent.STANDARD_INDENT;

    theOutputDestination . write ( " (\n" );

	/* Generate the arguments */
    privateGenerateCxxArguments ( theOutputDestination, null,
				  true,  /* Internal Task */
				  false, /* NOT MONITOR Task-Level */
				  true   /* ClearCxxExternalSubtaskName */ );

    theOutputDestination . write ( " )" );

	/* Stop indenting everything... */
    theOutputDestination . removeIndent ( indent );

  } /* public void generateCxxTaskInternal ( ... ) */



    /** Note: Certain constraints, such as ON_TERMINATE, contain additional
      *       subtasks.  For external generations, these additional subtasks
      *       are declared and allocated in this routine.
      * IMPORTANT:  This routine must be invoked exactly once before
      *             generateCxxTaskExternal() is invoked.  It sets state data
      *             that is used (and cleared) by generateCxxTaskExternal().
      * IMPORTANT:  This routine sets setCxxExternalSubtaskName, where it's
      *             available, for use with getCxxExternalSubtaskName().
      *             generateCxxTaskExternal WILL clear this value!
      *             (Exploited for label-based TDL_REFs in functions
      *              in DataSpawnTask.privateGenerateCxxOutsideOfTask().)
      * Addendum:   Changed to allow option non-automatic-clearing of
      *             setCxxExternalSubtaskName for Delayed Allocation.
      *             In these cases, it must be MANUALLY cleared!
      */
  public void generateCxxExternalSubtaskDeclarations (
					DataDestination  theOutputDestination,
					boolean          theAddLeadingNewline )
  {
    String  taskName  = null;
    String  taskLabel = null;

	/* Check for idiocy. */
    if ( getCxxExternalSubtaskName() != null )
    {
      System.err.println (
	    "[DataConstraint:generateCxxExternalSubtaskDeclarations]  "
	  + "Internal consistency check failure:  "
	  + "generateCxxExternalSubtaskDeclarations() has previously been "
	  + "invoked without an intervening generateCxxTaskExternal() call." );
    }

	/* Find out if we are doing anything */
    switch ( getConstraintType() )
    {
      case DataConstraint.ON_TERMINATE:
	if (   ( getOnTerminateTask()                 == null )
	    || ( getOnTerminateTask() . getTaskName() == null ) )
	{
	  throw new CompilationException ( "Internal Error:  "
		    + "DataConstraint object of type \"ON TERMINATE\""
		    + " is lacking a task to spawn." );
	}
	taskName  = getOnTerminateTask() . getTaskName();
	taskLabel
	= generateCxxExternalSubtaskDeclarations_internalGetOnTerminateLabel();
	break;


      default:
	taskName = null;
	break;
    }

    if ( taskName != null )
    {
      setCxxExternalSubtaskName (
		           DataComponent.CXX_TDL_ARBITRARY_SPAWN_TASK_NAME_LEAD
			 + DataSpawnTask.getArbitrarySubTaskIndex() );
      
	/* Declare/Allocate the spawnObject */
      if ( theAddLeadingNewline )
	theOutputDestination . write ( "\n" );

      theOutputDestination . write ( DataComponent.CXX_ALLOCATE_RETURN_VALUE );
      theOutputDestination . write ( "  " );
      theOutputDestination . write ( getCxxExternalSubtaskName() );
	/* If this is not a Delayed Allocation. */
      if ( getOnTerminateTask() . hasTaskExpression() == false )
      {
	theOutputDestination . write ( " = " );
	getOnTerminateTask()
	  . getTaskScope() . writeScope ( theOutputDestination );
	theOutputDestination
	  . write ( DataComponent.CXX_ALLOCATE_FUNCTION_LEAD);
	theOutputDestination . write ( taskName );
	theOutputDestination . write ( " ( \"" );
	theOutputDestination . write ( taskLabel );
	theOutputDestination . write ( "\" );\n" );
      }
      else
      {
	theOutputDestination . write ( ";\n" );
      }
    }
    else
    {
      setCxxExternalSubtaskName ( null );
    }
  }

  private String
    generateCxxExternalSubtaskDeclarations_internalGetOnTerminateLabel()
  {
    return 
        ""
      + getOnTerminateTask() . getTaskScope() . getAllScopeStrings()
      + getOnTerminateTask() . getTaskName()
      + "-onTerminate";
  }



	/* Ya know, those extern declarations can be labeled... */
  public DataLabelStatement getCxxExternalSubtaskDeclarationLabel()
  {
	/* Find out if we are doing anything */
    switch ( getConstraintType() )
    {
      case DataConstraint.ON_TERMINATE:
	if ( getOnTerminateTask() != null )
	  return getOnTerminateTask() . getLabel();

	/* NO BREAK */

      default:
	return null;
    }
  }



    /** Note:  This method generates the CXX equivalent code for this
      * constraint.  If theIsInIFStatement is true, it assumes that it
      * is generating constraints as clauses in the middle of a Gigantic
      * IF statement...
      *
      * Typically, this method is invoked
      * from DataSpawnTask's privateGenerateCxxOutsideOfTask(),
      * or from DataTaskDefinition's generateCxxAllocate().
      */
  public void generateCxxTaskExternal (
			     DataDestination  theOutputDestination,
			     String           theErrorLocation,
			     String           theSpawnObject,
			     String           theReturnValueString,
			     boolean          theIsInIFStatement,
			     boolean          theIsMonitorTaskLevelConstraint )
  {
    generateCxxTaskExternal ( theOutputDestination,
			      theErrorLocation,
			      theSpawnObject,
			      theReturnValueString,
			      theIsInIFStatement,
			      theIsMonitorTaskLevelConstraint,
			      true /* Clear setCxxExternalSubtaskName */ );
  }

  public void generateCxxTaskExternal (
			     DataDestination  theOutputDestination,
			     String           theErrorLocation,
			     String           theSpawnObject,
			     String           theReturnValueString,
			     boolean          theIsInIFStatement,
			     boolean          theIsMonitorTaskLevelConstraint,
			     boolean          theClearCxxExternalSubtaskName )
  {
    String    cxxFunction;
    int       i, commentIndent, argIndent;

	/* Find our Cxx Function */
    switch ( getConstraintType() )
    {
      case DataConstraint.EXPAND_FIRST:
	if ( theIsMonitorTaskLevelConstraint == false )
	  cxxFunction = DataComponent.CXX_TDL_EXPAND_FIRST;
	else
	  cxxFunction = DataComponent.CXX_TDL_EXPAND_FIRST_MONITOR_TASK_LEVEL;
	break;

      case DataConstraint.DELAY_EXPANSION:
	if ( theIsMonitorTaskLevelConstraint == false )
	  cxxFunction = DataComponent.CXX_TDL_DELAY_EXPANSION;
	else
	  cxxFunction
	    = DataComponent.CXX_TDL_DELAY_EXPANSION_MONITOR_TASK_LEVEL;
	break;

      case DataConstraint.PARALLEL:
	cxxFunction = DataComponent.CXX_TDL_PARALLEL;
	break;

      case DataConstraint.WAIT:
	cxxFunction = DataComponent.CXX_TDL_WAIT;
	break;

      case DataConstraint.SEQUENTIAL_HANDLING:
	cxxFunction = DataComponent.CXX_TDL_SEQUENTIAL_HANDLING;
	break;

      case DataConstraint.SEQUENTIAL_EXPANSION:
	if ( theIsMonitorTaskLevelConstraint == false )
	  cxxFunction = DataComponent.CXX_TDL_SEQUENTIAL_EXPANSION;
	else
	  cxxFunction
	    = DataComponent.CXX_TDL_SEQUENTIAL_EXPANSION_MONITOR_TASK_LEVEL;
	break;

      case DataConstraint.SEQUENTIAL_EXECUTION:
	if ( theIsMonitorTaskLevelConstraint == false )
	  cxxFunction = DataComponent.CXX_TDL_SEQUENTIAL_EXECUTION;
	else
	  cxxFunction
	    = DataComponent.CXX_TDL_SEQUENTIAL_EXECUTION_MONITOR_TASK_LEVEL;
	break;

      case DataConstraint.SERIAL:
	if ( theIsMonitorTaskLevelConstraint == false )
	  cxxFunction = DataComponent.CXX_TDL_SERIAL;
	else
	  cxxFunction = DataComponent.CXX_TDL_SERIAL_MONITOR_TASK_LEVEL;
	break;

      case DataConstraint.DISABLE_UNTIL_EVENT:
	cxxFunction = DataComponent.CXX_TDL_DISABLE_UNTIL_EVENT;
	break;

      case DataConstraint.DISABLE_UNTIL_TIME:
	cxxFunction = DataComponent.CXX_TDL_DISABLE_UNTIL;
	break;

      case DataConstraint.DISABLE_FOR_TIME:
	cxxFunction = DataComponent.CXX_TDL_DISABLE_FOR;
	break;

      case DataConstraint.TERMINATE_AT_EVENT:
	cxxFunction = DataComponent.CXX_TDL_TERMINATE_AT_EVENT;
	break;

      case DataConstraint.TERMINATE_AT_TIME:
	cxxFunction = DataComponent.CXX_TDL_TERMINATE_AT;
	break;

      case DataConstraint.TERMINATE_IN_TIME:
	cxxFunction = DataComponent.CXX_TDL_TERMINATE_IN;
	break;

      case DataConstraint.TERMINATE:
	cxxFunction = DataComponent.CXX_TDL_TERMINATE_IMMEDIATE;
	break;

      case DataConstraint.ACTIVATE_AT_EVENT:
	cxxFunction = DataComponent.CXX_TDL_ACTIVATE_AT_EVENT;
	break;

      case DataConstraint.ACTIVATE_AT_TIME:
	cxxFunction = DataComponent.CXX_TDL_ACTIVATE_AT;
	break;

      case DataConstraint.ACTIVATE_IN_TIME:
	cxxFunction = DataComponent.CXX_TDL_ACTIVATE_IN;
	break;

      case DataConstraint.ACTIVATE:
	cxxFunction = DataComponent.CXX_TDL_ACTIVATE_IMMEDIATE;
	break;

      case DataConstraint.MAXIMUM_ACTIVATE:
	cxxFunction = DataComponent.CXX_TDL_SET_MAXIMUM_ACTIVATE;
	break;

      case DataConstraint.MAXIMUM_TRIGGER:
	cxxFunction = DataComponent.CXX_TDL_SET_MAXIMUM_TRIGGER;
	break;

      case DataConstraint.MONITOR_PERIOD:
	cxxFunction = DataComponent.CXX_TDL_SET_MONITOR_PERIOD;
	break;

      case DataConstraint.EXCEPTION_HANDLER:
	cxxFunction = DataComponent.CXX_TDL_ADD_EXCEPTION_HANDLER;
	break;

      case DataConstraint.ON_TERMINATE:
	cxxFunction = DataComponent.CXX_TDL_ON_TERMINATE;
	break;


	/***************/
	/* ABORT CASES */
	/***************/
      default:
	String errorString = null;
	switch ( getConstraintType() )
	{
	  case DataConstraint.DISTRIBUTED_FORMAT:
	    errorString = "Distributed FORMAT";
	    break;
	  case DataConstraint.ON_AGENT:
	    errorString = "Distributed ON-AGENT";
	    break;
	  case DataConstraint.TCM_TASK_TREE_PARENT:
	    errorString = "PARENT";
	    break;
	  case DataConstraint.TDL_REF_IN:
	    errorString = "TDL_REF IN";
	    break;
	  case DataConstraint.TCM_TASK_TREE_NAME:
	    errorString = "NAME";
	    break;
	}
	if ( errorString != null )
	{
	  System.err.println ( "[DataConstraint:generateCxxTaskExternal]  "
			       + "Error:  Attempting to generate a "
			       + errorString
			       + " constraint.  This should not be "
			       + "happening.  Aborting this constraint..." );
	}
	else
	{
	  System.err.println ( "[DataConstraint:generateCxxTaskExternal]  "
			       + "Error:  Unknown Constraint-Type ("
			       + getConstraintType()
			       + ")  Aborting this constraint..." );
	}

	    /* ABORT */
	return;

    } /* switch ( getConstraintType() )  // Finding our Cxx Function */



	/***********************/
	/* Generate Constraint */
	/***********************/

	/* Write leading Comment describing this constraint */
    commentIndent = theOutputDestination . indentToCurrentColumn();
    theOutputDestination . setNewlineText ( "       // " );
    theOutputDestination . write ( "\n" );
    theOutputDestination . setStripLeadingWhitespace();
    generate ( theOutputDestination, DataComponent.ENTIRE_OBJECT );
    theOutputDestination . clearNewlineText();
    theOutputDestination . removeIndent ( commentIndent );

	/* Write Cxx Code */
    theOutputDestination . write ( "\n" );
    if ( theIsInIFStatement )
      theOutputDestination . write ( "&& " );
    theOutputDestination . write ( DataComponent.CXX_VERIFY_CONSTRAINT );
    theOutputDestination . write ( " (\n" );
    
	  /* Indent arguments... */
    
    theOutputDestination . addIndent ( 3 + DataComponent.STANDARD_INDENT * 2 );
    theOutputDestination . write ( theErrorLocation );
    theOutputDestination . write ( ",\n" );

    theOutputDestination . write ( "\"" );
    theOutputDestination . write ( cxxFunction );
    theOutputDestination . write ( "\",\n" );

    theOutputDestination . write ( theReturnValueString );
    theOutputDestination . write ( cxxFunction );

    theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT * 2);

    theOutputDestination . write ( " (\n" );


	/* Write the rest of the arguments. */
    privateGenerateCxxArguments ( theOutputDestination, theSpawnObject,
				  false,  /* External Task */ 
				  theIsMonitorTaskLevelConstraint,
				  theClearCxxExternalSubtaskName  );
    
	/* Stop Indenting arguments ... */
    theOutputDestination
      . removeIndent ( 3 + DataComponent.STANDARD_INDENT * 4 );

    theOutputDestination . write ( " ) )" );
    if ( theIsInIFStatement == false )
      theOutputDestination . write ( ";" );
    theOutputDestination . write ( "\n" );

  } /* public void generateCxxTaskExternal ( ... ) */



  protected void privateGenerateCxxArguments (
			     DataDestination  theOutputDestination,
			     String           theSpawnObject,
			     boolean          theIsInternalTask,
			     boolean          theIsMonitorTaskLevelConstraint,
			     boolean          theClearCxxExternalSubtaskName  )
  {
    DataTaskDefinition   task;
    String               onTerminateSpawnIdentifier;
    int                  constraintIndex;

	/* Indent arguments to this column ... */
    int   argIndent = theOutputDestination . indentToCurrentColumn();

	/* Generate our Arguments... */
    switch ( getConstraintType() )
    {
      case DataConstraint.EXPAND_FIRST:
      case DataConstraint.DELAY_EXPANSION:
      case DataConstraint.WAIT:
	if ( theSpawnObject != null )
	  theOutputDestination . write ( theSpawnObject );
	break;


      case DataConstraint.PARALLEL:
	    /* Nothing to generate for this... */
	break;


      case DataConstraint.SEQUENTIAL_EXPANSION:
      case DataConstraint.SEQUENTIAL_EXECUTION:
      case DataConstraint.SERIAL:
	if ( theIsMonitorTaskLevelConstraint )
	{
	  if ( theSpawnObject != null )
	    theOutputDestination . write ( theSpawnObject );
	  break;
	}

	/* NO BREAK!!! */

      case DataConstraint.SEQUENTIAL_HANDLING:
	generateEventTagTaskCxxReference ( theOutputDestination,
					   theSpawnObject,
					   theIsInternalTask );
	if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( ",\n" );
	  theOutputDestination . write ( theSpawnObject );
	}
	break;
	  

      case DataConstraint.DISABLE_UNTIL_EVENT:
      case DataConstraint.TERMINATE_AT_EVENT:
      case DataConstraint.ACTIVATE_AT_EVENT:
	    /* Generate our event (reference) arguments */
	privateGenerateCxxEventArguments ( theOutputDestination,
					   theSpawnObject,
					   theIsInternalTask );

	    /* const TCM_Task_Tree_Ref &  theNodeToConstrain */
	if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( ",\n" );
	  theOutputDestination . write ( theSpawnObject );
	}

	if ( getConstraintType() == DataConstraint.DISABLE_UNTIL_EVENT )
	{
	    /* const TCM_Interval_Enum &  theNodeToConstrainInterval */
	  theOutputDestination . write ( ",\n" );
	  theOutputDestination
	    . write ( getCxxInterval ( getConstraintOption() ) );
	}

	break;


      case DataConstraint.DISABLE_FOR_TIME:
      case DataConstraint.TERMINATE_IN_TIME:
      case DataConstraint.ACTIVATE_IN_TIME:

	   /* Write the AFTER reference */
	if ( getHasEventTagTask() )
	{
	    /* Generate our event (reference) arguments */
	  privateGenerateCxxEventArguments ( theOutputDestination,
					     theSpawnObject,
					     theIsInternalTask );
	  theOutputDestination . write ( ",\n" );
	}

	    /* ***** !!!!!  NO BREAK !!!!! ***** */

      case DataConstraint.DISABLE_UNTIL_TIME:
      case DataConstraint.TERMINATE_AT_TIME:
      case DataConstraint.ACTIVATE_AT_TIME:

	    /* const TCM_Task_Tree_Ref &  theNodeToConstrain */
	if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( theSpawnObject );
	  theOutputDestination . write ( ",\n" );
	}

	if (   ( getConstraintType() == DataConstraint.DISABLE_UNTIL_TIME )
	    || ( getConstraintType() == DataConstraint.DISABLE_FOR_TIME   ) )
	{
	    /* const TCM_Interval_Enum &  theNodeToConstrainInterval */
	  theOutputDestination
	    . write ( getCxxInterval ( getConstraintOption() ) );
	  theOutputDestination . write ( ",\n" );
	}

	generateCxxTimeArguments ( theOutputDestination );
	break;



      case DataConstraint.TERMINATE:
      case DataConstraint.ACTIVATE:

	    /* const TCM_Task_Tree_Ref &  theNodeToConstrain */
	if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( theSpawnObject );
	}

	break;



      case DataConstraint.MAXIMUM_ACTIVATE:
      case DataConstraint.MAXIMUM_TRIGGER:

	    /* const TCM_Task_Tree_Ref &  theNodeToConstrain */
	if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( theSpawnObject );
	  theOutputDestination . write ( "," ); /* Delay writing the \n */
	}

	if ( theOutputDestination . getEnableLineMacros() == true )
	{
	  theOutputDestination . setUsingTdlFileName ( true );
	  theOutputDestination
	    . makeNextLineNumber  ( getNumericExpression() . getLineNumber() );
	  theOutputDestination . write ( "\n" ); /* Flush #line macro */
	}
	else if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( "\n" ); /* We will still need a \n */
	}

	theOutputDestination . setStripLeadingWhitespace();
	theOutputDestination . setPersistentlyStripLeadingSpaces ( true );

	getNumericExpression() . generate ( theOutputDestination,
					    DataComponent.ENTIRE_OBJECT );

	theOutputDestination . setPersistentlyStripLeadingSpaces ( false );

	if ( theOutputDestination . getEnableLineMacros() == true )
	{
	  theOutputDestination . setUsingTdlFileName ( false );
	  theOutputDestination . write ( "\n" ); /* Flush #line macro */
	}

	break;



      case DataConstraint.MONITOR_PERIOD:

	    /* const TCM_Task_Tree_Ref &  theNodeToConstrain */
	if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( theSpawnObject );
	  theOutputDestination . write ( ",\n" );
	}

	generateCxxTimeArguments ( theOutputDestination );
	break;



      case DataConstraint.EXCEPTION_HANDLER:
	    /* These arguments will be long...  Use next-line indenting... */
	theOutputDestination . removeIndent ( argIndent );
	theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );
	//Unnecessary: theOutputDestination . write ( "\n" );


	if ( theSpawnObject != null )
	{
	  theOutputDestination . write ( theSpawnObject );
	  theOutputDestination . write ( ",\n" );
	}

		/* Exception Handler argument */
	if (   ( getExceptionHandlerTask()                            == null )
	    || ( getExceptionHandlerTask() . getTaskName()            == null )
	    || ( getExceptionHandlerTask() . getTaskName() . length() <= 0    )
	    )
	{
	  throw new CompilationException ( "Internal Error:  DataConstraint "
				       + "object of type \"EXCEPTION HANDLER\""
				       + " does not handle an exception." );
	}

	getExceptionHandlerTask()
	  . generateTask ( theOutputDestination,
			   DataComponent.ENTIRE_OBJECT,
			   false,  /* No leading space    */
			   false,  /* No leading comments */
			   true,   /* Indent nicely,      */
			   true,   /*   on next line.     */
			   true,   /* Is Cxx generation   */
			   DataComponent.CXX_CREATE_HANDLER_FUNCTION_LEAD );

		/* Maximum Invocations argument */
	  /* Currently there is no Maximum Invocations argument */


	theOutputDestination . write ( ",\n" );

	    /* Override in the special case of a Constraint-Statement *
	     * based on THIS.  E.g.:  "THIS constraint"               */
	if (    ( getParent() instanceof DataConstraintStatement )
	     && (  ( (DataConstraintStatement) getParent() )
		        . getHasTaskTagOfThis()                  ) )
	{
	  constraintIndex = DataComponent.UNKNOWN_CONSTRAINT_INDEX;
	}
	else
	{
	    /* Find our top-most object -- the Task that we are a part of */
	  task = getParentTaskDefinition();

	  if (   ( task != null        )
	      && ( task != getParent() ) )
	    constraintIndex = task . getIdentifierForConstraint ( this, true );
	  else
	    constraintIndex = DataComponent.UNKNOWN_CONSTRAINT_INDEX;
	}

	theOutputDestination . write ( Integer.toString ( constraintIndex ) );


	    /* Restore normal argument indenting */
	theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
	theOutputDestination . addIndent    ( argIndent                     );
	break;




      case DataConstraint.ON_TERMINATE:
	    /* These arguments will be long...  Use next-line indenting... */
	theOutputDestination . removeIndent ( argIndent );
	theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );
	// Unnecessary: theOutputDestination . write ( "\n" );

	    /* Idiocy checks */
	if (   ( getOnTerminateTask()                 == null )
	    || ( getOnTerminateTask() . getTaskName() == null ) )
	{
	    throw new CompilationException ( "Internal Error:  "
		    + "DataConstraint object of type \"ON TERMINATE\""
		    + " is lacking a task to spawn." );
	}


	    /* If external task */
	if ( theIsInternalTask == false )
	{
	  if ( theSpawnObject != null )
	  {
	    theOutputDestination . write ( theSpawnObject );
	    theOutputDestination . write ( ",\n" );
	  }
	  else
	  {
	    throw new CompilationException ( "Internal Error:  "
		    + "External DataConstraint object of type \"ON TERMINATE\""
		    + " is lacking an object being spawned." );
	  }

	  if ( getCxxExternalSubtaskName() == null )
	  {
	    throw new CompilationException ( "Internal Error:  "
		    + "generateCxxExternalSubtaskDeclarations() was "
		    + "NOT invoked before generateCxxTaskExternal().  "
		    + "NO allocation was made for this \"ON TERMINATE\" "
		    + "constraint's subtask." );
	  }

	  theOutputDestination
	    . write ( DataComponent.CXX_DO_SET_ON_TERMINATE_TASK_ACTION );
	  theOutputDestination . write ( " ( " );

	  theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );
	  theOutputDestination . write ( "\n" );

	  theOutputDestination . write ( "\"[:'" );
	  theOutputDestination . write ( getOnTerminateTask() . getTaskName());
	  theOutputDestination . write ( "' - " );
	  theOutputDestination . write ( getCxxExternalSubtaskName() );
	  theOutputDestination . write ( "]\",\n" );

	  theOutputDestination . write ( getCxxExternalSubtaskName() );
	  theOutputDestination . write ( ",\n" );

		/* Add this extra argument since we are using create-action */
	  getOnTerminateTask() . addArgument ( getCxxExternalSubtaskName(), 0);

	  if ( theClearCxxExternalSubtaskName )
	    setCxxExternalSubtaskName ( null );
	}   /* IF ( theIsInternalTask == false ) */
	else /* Ie: theIsInternalTask == true */
	{
	    /* Find our top-most object -- the Task that we are a part of */
	  task = getParentTaskDefinition();

	    /* If we don't have a parent task. */
	  if ( task == null )
	  {
	    throw new CompilationException (
	      "Internal Error in [DataConstraint:privateGenerateCxxArguments]:"
	      + "  On-Terminate Constraint is not contained in a task.  "
	      + "Unable to satisfy constraint \"" + toString() + "\"." );
	  }

	    /* Find our default spawn-identifier */
	  onTerminateSpawnIdentifier
	    = task . getIdentifierForSubtask ( getOnTerminateTask() );


	  theOutputDestination
	    . write ( DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE );
	  theOutputDestination . write ( " . " );
	  theOutputDestination . write (
	     DataComponent.CXX_TDL_TASK_HANDLER_DO_SET_ON_TERMINATE_TASK_ACTION
	     );
	  theOutputDestination . write ( " (" );

	  theOutputDestination . addIndent ( DataComponent.STANDARD_INDENT );
	  theOutputDestination . write ( "\n" );
	  
	  theOutputDestination . write ( "\"" );
	  theOutputDestination . write ( onTerminateSpawnIdentifier );
	  theOutputDestination . write ( "\",\n" );

		/* Add these extra arguments since we are using create-action*/
	  getOnTerminateTask() . addArgument ( 
	      DataComponent.CXX_TDL_TASK_HANDLER_INSTANCE
		+ " . " + DataComponent.CXX_TDL_TASK_HANDLER_GET_TASK_TREE_REF
		+ " ( \"" + onTerminateSpawnIdentifier + "\" )",
	      0 );
	}

	    /* Handle Delayed Allocation */
	getOnTerminateTask() . addArgument ( 
	  DataComponent
	    . CXX_INVOKE_AS_LOCAL_NONDISTRIBUTED_ONLY_DELAYED_ALLOCATION
	 + " ( \""
	 + generateCxxExternalSubtaskDeclarations_internalGetOnTerminateLabel()
	 + "\", "
	 + (   ( getOnTerminateTask() . hasTaskExpression() )
	     ? "TRUE" : "FALSE" )
	 + " )",
	 1 );

	getOnTerminateTask()
	  . generateTask ( theOutputDestination,
			   DataComponent.ENTIRE_OBJECT,
			   false,  /* No leading space    */
			   false,  /* No leading comments */
			   true,   /* Indent nicely,      */
			   true,   /*   on next line.     */
			   true,   /* Is Cxx generation   */
			   DataComponent.CXX_CREATEACTION_FUNCTION_LEAD );
	theOutputDestination . write ( " )" );
	theOutputDestination . removeIndent( DataComponent.STANDARD_INDENT );

	    /* Remove the extra arguments we just added. */
	getOnTerminateTask() . removeArgument ( 0 );
	getOnTerminateTask() . removeArgument ( 0 );


	    /* Restore normal argument indenting */
	theOutputDestination . removeIndent ( DataComponent.STANDARD_INDENT );
	theOutputDestination . addIndent    ( argIndent                     );
	break;



      case DataConstraint.ON_AGENT:
		/* Generate "AGENT" DataExpression */
	if ( getDistributedOnAgentExpression() != null )
	  getDistributedOnAgentExpression()
	    . generate ( theOutputDestination, DataComponent.ENTIRE_OBJECT );
	else
	  throw new CompilationException ( "Internal Error:  DataConstraint "
	    + "object of type \"ON AGENT\" has a null on-agent expression." );
	break;


      default:
	System.err.println ( "[DataConstraint:privateGenerateCxxArguments]  "
			     + "ERROR:  Unknown Constraint-Type ("
			     + getConstraintType() + ")" );
	break;

    } /* switch ( getConstraintType() ) // Generating our Arguments*/


	/* Stop indenting Arguments... */
    theOutputDestination . removeIndent ( argIndent );

  } /*  protected void privateGenerateCxxArguments ( ... ) */


	/* Note:  Strip leading zeros off hours, minutes, & seconds
	 * Otherwise C++ will think that they are octal.
	 */
  public String getCxxHours()
  {
    return (    getHasHours()
	     ?  DataConstraint.stripLeadingZeros ( getHours().trim() )
	     :  "0" );
  }

  public String getCxxMinutes()
  {
    return (    getHasMinutes()
	     ?  DataConstraint.stripLeadingZeros ( getMinutes().trim() )
	     :  "0" );
  }

  public String getCxxSeconds()
  {
    return (    getHasSeconds()
	     ?  DataConstraint.stripLeadingZeros ( getSeconds().trim() )
	     :  "0" );
  }

  public String getCxxFractionsOfASecond()
  {
    return (    getHasFractionsOfASecond()
	     ? ( "0." + getFractionsOfASecond().trim() )
	     : "double(0)" );
  }


  public void generateCxxTimeArguments ( DataDestination theOutputDestination )
  {
    generateCxxTimeArguments ( theOutputDestination,
			       "", "", "", "", ", ", true );
  }

  public void generateCxxTimeArguments (
			       DataDestination theOutputDestination,
			       String          theLeadExpressionString,
			       String          theTailExpressionString,
			       String          theLeadTimeString,
			       String          theTailTimeString,
			       String          theTimeSeparatorString,
			       boolean         theIndentFirstLineOfExpression )
  {
    int  indent, unindent;

    if ( getTimeExpression() != null )
    {
      theOutputDestination . write ( theLeadExpressionString );
      theOutputDestination
	. write ( DataComponent.CXX_TDL_TRANSLATE_SECONDS_TO_MSECS_FUNCTION );
      theOutputDestination . write ( " ( " );

      indent = theOutputDestination . indentToCurrentColumn ( );

      if ( theOutputDestination . getEnableLineMacros() == true )
      {
	theOutputDestination . setUsingTdlFileName ( true );
	theOutputDestination
	  . makeNextLineNumber  ( getTimeExpression() . getLineNumber() );
	theOutputDestination . write ( "\n" ); /* Flush #line macro */
      }

	/* Unindent by the time-expression's leading whitespace */
      unindent = getTimeExpression() . getLeadingWhitespaceIndent();
      if ( theOutputDestination . getIndent() > unindent )
      {
	theOutputDestination . removeIndent ( unindent );
	indent -= unindent;
      }
	/* Unfortunately, usually we need the first line of the time-expression
	 * indented.  But not any remaining lines...  *sigh*
	 */
      if ( theIndentFirstLineOfExpression == true )
      {
	for ( ; unindent > 0;  unindent -- )
	  theOutputDestination . write ( " " );
      }

      theOutputDestination . setStripLeadingWhitespace();
      getTimeExpression() . generate ( theOutputDestination,
				       DataComponent.ENTIRE_OBJECT );

      if ( theOutputDestination . getEnableLineMacros() == true )
      {
	theOutputDestination . setUsingTdlFileName ( false );
	theOutputDestination . write ( "\n" ); /* Flush #line macro */
      }

      theOutputDestination . write ( " )" );
      theOutputDestination . write ( theTailExpressionString );

      theOutputDestination . removeIndent ( indent );
    }
    else
    {
      theOutputDestination . write ( theLeadTimeString );

      indent = theOutputDestination . indentToCurrentColumn ( );

	    /* u_int4                     theHours */
      theOutputDestination . write ( getCxxHours() );
      theOutputDestination . write ( theTimeSeparatorString );

	    /* u_int4                     theMinutes */
      theOutputDestination . write ( getCxxMinutes() );
      theOutputDestination . write ( theTimeSeparatorString );

	    /* u_int4                     theSeconds */
      theOutputDestination . write ( getCxxSeconds() );
      theOutputDestination . write ( theTimeSeparatorString );

	    /* double                     theFractionsOfASecond */
      theOutputDestination . write ( getCxxFractionsOfASecond() );

      theOutputDestination . removeIndent ( indent );

      theOutputDestination . write ( theTailTimeString );
    }
  }


  public String getCxxStateBoundary ( int theStateBoundary )
  {
    switch ( theStateBoundary )
    {
      case DataConstraint.ENABLED:
	                          return DataComponent.CXX_TCM_ENABLED_STATE;
      case DataConstraint.ACTIVE:
	                          return DataComponent.CXX_TCM_ACTIVE_STATE;
      case DataConstraint.COMPLETED:
	                          return DataComponent.CXX_TCM_COMPLETED_STATE;
      default:
	return DataComponent.CXX_TCM_UNKNOWN_STATE;
    }
  }

  public String getCxxInterval ( int theConstraintOption )
  {
    switch ( theConstraintOption )
    {
      case DataConstraint.HANDLING:
	return DataComponent.CXX_TCM_HANDLING_INTERVAL;

      case DataConstraint.EXPANSION:
	return DataComponent.CXX_TCM_EXPANSION_INTERVAL;

      case DataConstraint.EXECUTION:
	return DataComponent.CXX_TCM_EXECUTION_INTERVAL;

      default:
	return DataComponent.CXX_TCM_UNKNOWN_INTERVAL;
    }
  }



  public void generateEventTagTaskCxxReference ( 
					DataDestination  theOutputDestination,
					String           theSpawnObject,
					boolean          theIsInternalTask )
  {
    if ( getHasEventTagTaskOfThis() )
    {
      if ( theIsInternalTask )
	theOutputDestination
	  . write ( DataComponent.CXX_ENCLOSING_TASK_CONSTRAINT_OBJECT );
      else
	theOutputDestination . write ( DataComponent.CXX_TCM_ROOT_NODE );
    }

    else if ( getHasEventTagTaskOfChild() )
    {
      if ( theIsInternalTask )
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_CONSTRAINT_CHILD_TASK_REF );
      else
	theOutputDestination . write ( theSpawnObject );
    }

    else if ( getHasEventTagTaskOfPrevious() )
    {
      if ( theIsInternalTask )
	theOutputDestination
	  . write ( DataComponent.CXX_TDL_PREVIOUS_INTERNAL_TASK_REF );
      else
	theOutputDestination
	  . write ( DataComponent.CXX_TCM_PREVIOUS_EXTERNAL_TASK_REF );
    }

    else
    {
	/* Generate the CXX Reference. */
      DataStatement.generateTagTaskCxxReference ( theOutputDestination,
						  getEventTagTask(),
						  getEventTagTaskIndexes(),
						  this );
    }
  }


  protected void privateGenerateCxxEventArguments (
					DataDestination  theOutputDestination,
					String           theSpawnObject,
					boolean          theIsInternalTask )
  {
	    /* const TCM_Task_Tree_Ref &  theReferenceNode */
    generateEventTagTaskCxxReference ( theOutputDestination,
				       theSpawnObject,
				       theIsInternalTask );

	    /* const TCM_Interval_Enum &  theReferenceInterval */
    theOutputDestination . write ( ",\n" );
    theOutputDestination
      . write ( getCxxInterval ( getEventConstraintOption() ) );

	    /* const TCM_Point_Enum    &  theReferenceStateBoundary */
    theOutputDestination . write ( ",\n" );
    theOutputDestination
      . write ( getCxxStateBoundary ( getStateBoundary() ) );
  }

}
