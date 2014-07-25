
/**
 *  In a nutshell:  DataComponent is the abstract base class of all our
 *  Data-Components...  It handles certain common, tedious functionality
 *  such as:
 *    - Keeping track of parent-child relationships.
 *    - Storing non-significant tokens (Strings and/or DataComponents)
 *       as subcomponents.  (Typically comments, etc...)
 *    - Storing indexes into this Vector of subcomponents.
 *       (These indexes can be (are) used by subclasses to indicate where
 *        those subclasses should insert their significant tokens.)
 *    - Provides methods for generate()'ing sets of subcomponents.
 *    - Provides toString() wrapper around generate()
 *    - Provides isValid() and generate(...) interfaces.
 *
 *    - Providing a location for key "global" values to be defined.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 *
 */

public abstract class DataComponent extends Object
{
	/* Class Constants */
  public final static String EMPTY_STRING        = "";
  public final static String SPACE_STRING        = " ";
  public final static int    INVALID_INDEX       = -1; 
  public final static int    INVALID_LINE_NUMBER = -101; /* Arbitrary */
  public final static String FIRST_TOKEN_INDEX ="DataComponentFirstTokenIndex";

		/* Default value to lead-off error/warning messages */
  public final static String DEFAULT_MESSAGE_FILENAME_LEAD       = "Line:";
		/* Default data-source when there's no filename. */
  public final static String DEFAULT_DATA_SOURCE_NAME            = "stdin";
  public final static String DEFAULT_DATA_SOURCE_NAME_WITH_COLON = "stdin:";


		/* Used to generate() the TDL code. */
  public final static int    ENTIRE_OBJECT      = 0;

		/* Used for generate()'ing HTML Documentation */

  public final static int    HTML_DOCUMENTATION = 2000;

		/* Used for generate()'ing C++ code */

	/* Header = allocate();  class{};         */
	/* Code   = allocate(){} class::execute() */            /* [TDLC -2] */
  public final static int    CXX_HEADER                   = 1001;
  public final static int    CXX_CODE                     = 1002;

	/* Header = inline allocate(){}  class{ execute(){} }; */
	/* Code does not exist.                                */
  public final static int    INLINED_CXX_CODE             = 1003;

	/* Header = inline allocate(){} class{}; */
	/* Code   = class::execute()             */             /* [TDLC -1] */
  public final static int    CXX_HEADER_INLINED_FUNCTIONS = 1004;
  public final static int    CXX_CODE_NO_FUNCTIONS        = 1005;

	/* Header = allocate();                            */
	/* Code   = allocate(){} class{}; class::execute() */   /* [TDLC -3] */
  public final static int    CXX_BARE_HEADER              = 1006;
  public final static int    CXX_CODE_AND_HEADER          = 1007;
  
	/* Code   = allocate(){} class{}; class::execute() */   /* [TDLC -4] */
	/* (This can *ONLY* be used inside TDLC.java!)     */
  public final static int    CXX_CODE_AND_HEADER_SEQUENTIAL = 1008;


	/* Generate distributed-invocation-only. */
  public final static int    CXX_HEADER_INLINED_DISTRIBUTED_ONLY = 1009; /*-1*/
  public final static int    CXX_HEADER_DISTRIBUTED_ONLY         = 1010; /*-2*/
  public final static int    CXX_CODE_DISTRIBUTED_ONLY           = 1011; /*-2*/


	/* Generation of distributed exceptions code. */
  public final static int    CXX_DISTRIBUTED_EXCEPTIONS_ONLY     = 1012;
  public final static int    CXX_DISTRIBUTED_EXCEPTIONS_NONE     = 1013;

	/* These are used to convert from the numeric constants to strings */
	/* See the Class-Method  getCxxSubsetName() */
  protected final static String[] CXX_STRINGS
	= { "CXX_HEADER",                   "CXX_CODE",
	    "INLINED_CXX_CODE",
	    "CXX_HEADER_INLINED_FUNCTIONS", "CXX_CODE_NO_FUNCTIONS",
	    "CXX_BARE_HEADER",              "CXX_CODE_AND_HEADER",
	    "CXX_CODE_AND_HEADER_SEQUENTIAL" };


		/* Used to create dynamic string-buffer for distributed task
		 * argument-format configuration information.
		 */
  public final static String CXX_DYNAMIC_STRING_BUFFER = "TDL_StringBuffer";

		/* Used to perform malloc() with error-checking. */
  public final static String CXX_MALLOC_FUNCTION = "MALLOC";

		/* Used for generating unique C++ names */
  public final static String CXX_NAME_LEAD          = "_TDL_";
  public final static String CXX_EXCEPTION_NAME_LEAD= "_TDL_Exception_";
  public final static String CXX_HANDLER_NAME_LEAD  = "_TDL_ExceptionHandler_";

		/* For auto-generating #define'd distributed FORMAT strings. */
  public final static String CXX_DISTRIBUTED_FORMAT_TRAILER = "_IPC_FORMAT";

		/* For embedded structs/enums. */
  public final static String CXX_DISTRIBUTED_QUALIFIED_CLASSNAME_SEPARATOR
				= "__";

		/* Arbitrary (non-task-based) spawn-task */
  public final static String CXX_TDL_ARBITRARY_SPAWN_TASK_NAME_LEAD
				=   DataComponent.CXX_NAME_LEAD
                                  + "arbitrarySubTask_";

		/* Means by which users access underlying TCM task references*/
  public final static String CXX_TDL_REF = "TDL_REF";

		/* TDL keyword to bind TCM Tasks */
  public final static String TDL_BIND
				=   "TDL_BIND";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_BIND_TCM_TASK
				=   "bindTCMTask";


		/* Base class that all produceCxx()'ed code inherits from. */
  public final static String CXX_BASE_CLASS           = "_TDL_BaseAction";
  public final static String CXX_EXCEPTION_BASE_CLASS = "_TDL_BaseException";
  public final static String CXX_HANDLER_BASE_CLASS   = "_TDL_BaseHandler";
  public final static String CXX_TCM_EXCEPTION_CLASS  = "TCM_Exception";

		/* Note: This corresponds to the root node in TCM. */
		/* It was TCM_RootNode().
		 * Then it became _TDL_ENCLOSING_TASK.
		 * And now it's TCM_GetCurrentTaskTreeRefForThisThread().
		 */
 public       static String CXX_TCM_ROOT_NODE
				=   "TCM_GetCurrentTaskTreeRefForThisThread()";

  public static void restorePreviousRootNodeValueForBackwardCompatibility()
  {
    CXX_TCM_ROOT_NODE = "::" + DataComponent.CXX_ENCLOSING_TASK_REF;
    CXX_TCM_PREVIOUS_EXTERNAL_TASK_REF
      =   "TCM_LastChild ( " + DataComponent.CXX_TCM_ROOT_NODE + " )";
  }


		/* TCM - Allocate - Goal function */
  public final static String CXX_TCM_ALLOCATE_GOAL_FUNCTION
				=   "TCM_AllocateGoalNode";

		/* TCM - Allocate - Command function */
  public final static String CXX_TCM_ALLOCATE_COMMAND_FUNCTION
				=   "TCM_AllocateCommandNode";

		/* TCM - Allocate - Monitor function */
  public final static String CXX_TCM_ALLOCATE_MONITOR_FUNCTION
				=   "TCM_AllocateCompleteMonitorNode";

		/* TCM - Allocate - Distributed function */
  public final static String CXX_TCM_ALLOCATE_DISTRIBUTED_FUNCTION
				=   "TCM_AllocateDistributedNode";

		/* TCM - Check if Task-Tree-Node is distributed function */
  public final static String CXX_CHECK_IF_TASK_DISTRIBUTED
				=   "_TDL_DO_CHECK_IF_TASK_DISTRIBUTED";

		/* TCM - SetAction function */
  public final static String CXX_TCM_SET_ACTION_FUNCTION
				=   "TCM_SetActualAction";

		/* TCM - Insert-node (start-node) function */
  public final static String CXX_TCM_INSERT_NODE_FUNCTION
				=   "TCM_InsertNode";

		/* TCM - Set Persistence function */
  public final static String CXX_TCM_SET_PERSISTENCE
				=   "TCM_SetPersistence";

		/* TCM - Set Threaded function */
		/* If this is the TCM function directly, and NOT an interface
		 * function, we are no longer dependent on threading UNLESS
		 * we are compiling threaded tasks, or TCM is compiled with
		 * threading enabled.
		 */
  public final static String CXX_TCM_SET_THREADED
				=   "TCM_SetIsThreadedTask";


		/* TDL files - Macro to prevent variable-not-used warnings. */
  public final static String CXX_MARK_AS_USED_TO_COMPILER
				=   "_TDL_MARKUSED";

		/* TDL files - Macro to invoke Resume() */
  public final static String CXX_TDL_RESUME_INVOCATION_MACRO
				=   "_TDL_INTERNAL_RESUME";

		/* TDL - Translate Seconds to MSecs function */
  public final static String CXX_TDL_TRANSLATE_SECONDS_TO_MSECS_FUNCTION
				=   "_TDL_SecondsToMSecs";

		/* Monitor TCM-action defaults. */
  public final static String CXX_TCM_DEFAULT_MAX_ACTIVATES
				=   "_TDL_MAXIMUM_ACTIVATES";


		/* TCM - Error return value from TCM functions */
  public final static String CXX_TCM_ERROR
				=   "TCM_Error";

		/* TCM - Function to Indicate successful completion of a Task*/
  public final static String CXX_TCM_TASK_COMPLETED_SUCCESSFULLY
				=   "_TDL_DO_TCM_SUCCESS";

		/* TCM - Failure function */
  public final static String CXX_TCM_FAIL
				=   "_TDL_DO_TCM_FAIL";

		/* TCM - Exception bypass function */
  public final static String CXX_TCM_BYPASS
				=   "_TDL_DO_TCM_BYPASS";


		/* TCM - Constants: */
  public final static String CXX_TCM_ENABLED_STATE      = "Start_Point";
  public final static String CXX_TCM_ACTIVE_STATE       = "Start_Point";
  public final static String CXX_TCM_COMPLETED_STATE    = "End_Point";
  public final static String CXX_TCM_UNKNOWN_STATE      = "Unknown_Point";
  public final static String CXX_TCM_HANDLING_INTERVAL  = "Handling_Interval";
  public final static String CXX_TCM_EXPANSION_INTERVAL = "Planning_Interval";
  public final static String CXX_TCM_EXECUTION_INTERVAL = "Achieving_Interval";
  public final static String CXX_TCM_UNKNOWN_INTERVAL   = "Unknown_Interval";

		/* TCM - XOR constraint constants */
  public final static String CXX_TCM_BASE_XOR_VALUE         = "0";
  public final static String CXX_TCM_EXPAND_FIRST_XOR_VALUE = "PLAN_FIRST";
  public final static String CXX_TCM_DELAY_EXPANSION_XOR_VALUE
							    = "DELAY_PLANNING";
  public final static String CXX_TCM_SEQUENTIAL_EXPANSION_XOR_VALUE
							    = "SEQ_PLANNING";
  public final static String CXX_TCM_SEQUENTIAL_EXECUTION_XOR_VALUE
							    = "SEQ_ACH";
  public final static String CXX_TCM_SERIAL_XOR_VALUE
				=    CXX_TCM_DELAY_EXPANSION_XOR_VALUE + " | "
				   + CXX_TCM_SEQUENTIAL_EXECUTION_XOR_VALUE;


		/* TDL function for verifying allocation */
  public final static String CXX_VERIFY_ALLOCATION
				=   "TDL::verifyAllocation";

		/* TDL function for verifying setAction */
  public final static String CXX_VERIFY_SET_ACTION
				=   "TDL::verifySetAction";

		/* TDL function for verifying Insert-Node */
  public final static String CXX_VERIFY_INSERT_NODE
				=   "TDL::verifyInsertNode";

		/* TDL function for verifying constraints */
  public final static String CXX_VERIFY_CONSTRAINT
				=   "TDL::verifyConstraint";

		/* TDL function for doing on-terminate-set-action. */
  public final static String CXX_DO_SET_ON_TERMINATE_TASK_ACTION
				=   "TDL::doSetOnTerminateTaskAction";


		/* TDL function for testing if two strings are equal. */
  public final static String CXX_TDL_STRING_EQUAL
				=   "StringEqual";


		/* TDL Spawned-Task Handler object class name */
  public final static String CXX_TDL_TASK_HANDLER_CLASS
				=   "_TDL_HandleManager";

		/* TDL Trivial Spawned-Task Handler object class name */
		/* (Used for TDL_REF in _TDL_CreateAction_...)        */
  public final static String CXX_TDL_TRIVIAL_TASK_HANDLER_CLASS
				=   "_TDL_TrivialHandleManager";

		/* TDL Spawned-Task Handler object instance name */
  public final static String CXX_TDL_TASK_HANDLER_INSTANCE
				=   "_TDL_SpawnedTasks";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_ADD_SPAWN_DISTRIBUTED_METHOD
				=   "addSpawnStatement_Distributed";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_ADD_SPAWN_LOCAL_METHOD
				=   "addSpawnStatement_Local";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_ADD_SPAWN_EITHER_METHOD
				=   "addSpawnStatement_Either";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_ADD_SPAWN_DELAYED_ALLOCATION_METHOD
				=   "addSpawnStatement_DelayedAllocation";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_ADD_WITH_METHOD
				=   "addWithStatement";

		/* TDL Spawned-Task Handler sub-object instance method */
  public final static String CXX_TDL_TASK_HANDLER_DO_ADD_CONSTRAINT
				=   "doAddWithStatementConstraint";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_PUSH_WITH_METHOD
				=   "pushWithStatement";

		/* TDL Spawned-Task Handler method return value */
  public final static String CXX_TDL_TASK_HANDLER_PUSH_WITH_SUCCESSFULLY
				=   "SUCCESS";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_POP_WITH_METHOD
				=   "popWithStatement";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_PUSH_ITERATION_METHOD
				=   "pushIteration";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_INCREMENT_ITERATION_METHOD
				=   "incrementIteration";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_POP_ITERATION_METHOD
				=   "popIteration";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_ADD_NAME_METHOD
				=   "addName";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_SET_DEFAULT_WITH_PARENT
				=   "setDefaultWithParent";

		/* TDL Spawned-Task Handler object instance method */
  public final static String 
			CXX_TDL_TASK_HANDLER_SET_TCM_TASK_TREE_NODE_NAME_METHOD
				=   "setTcmTaskTreeNodeName";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_DESTROY_METHOD
				=   "destroy";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_DESTROY_IF_NOT_RUNNING_METHOD
				=   "destroyIfUnused";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_DO_START_INVOKING
				=   "startInvokingSpawn";

		/* TDL Spawned-Task Handler sub-object instance method */
  public final static String CXX_TDL_TASK_HANDLER_DO_SET_ACTION
				=   "doSetAction";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_DO_APPLY_CONSTRAINT
				=   "doApplyConstraint";

		/* TDL Spawned-Task Handler sub-object instance method */
  public final static String CXX_TDL_TASK_HANDLER_APPLY_CONSTRAINT_TO
				=   "applyConstraintTo";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_DO_INSERT
				=   "doInsertSpawn";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_FINISH_INVOKING
				=   "finishInvokingSpawn";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_GET_CURRENT_TASK_REF
				=   "getCurrentTaskTcmTaskTreeRef";

		/* TDL Spawned-Task Handler object instance method */
  public final static String
			   CXX_TDL_TASK_HANDLER_DO_SET_ON_TERMINATE_TASK_ACTION
				=   "doSetOnTerminateTaskAction";

		/* TDL Spawned-Task Handler object instance method */
  public final static String CXX_TDL_TASK_HANDLER_GET_TASK_TREE_REF
				=   "getTaskTreeRef";



		/* TDL Task-Iteration selection instance method */
  public final static String CXX_TDL_RESET_CACHED_LOOKUP
				=   "resetCachedLookup";

		/* TDL Task-Iteration selection instance method */
  public final static String CXX_TDL_CACHE_DESCEND_INTO_BRANCH
				=   "cacheDescendIntoBranch";

		/* TDL Task-Iteration selection instance method */
  public final static String CXX_TDL_CACHE_DESCEND_INTO_NODE
				=   "cacheDescendIntoNode";

		/* TDL Task-Iteration selection instance method */
  public final static String CXX_TDL_GET_CACHED_TREE_NODE
				=   "getCachedTreeNodeNonConst";

		/* TDL Task-Iteration selection instance method */
  public final static String CXX_TDL_GET_CACHED_TOPMOST_TREE_NODE_BRANCH
				=   "getCachedTopmostTreeNodeBranch";

		/* TDL Task-Iteration selection instance method */
  public final static String CXX_TDL_GET_CURRENT_ARRAY_INDEX
				=   "getCurrentIterationIndex";

		/* TDL Task-Iteration default array index */
  public final static String CXX_TDL_DEFAULT_ARRAY_INDEX
				=   "_TDL_TreeNode::NO_ARRAY_INDEX";



		/* TDL Keyword */
  public final static String THIS = "THIS";



		/* Note: This corresponds to the PARENT keyword in the TDL
		 * language, and the PARENT constrainer-tag in DataConstraint.
		 *
		 * Note:  _TDL_ENCLOSING_TASK is a global variable that
		 * points to TCM_RootNode().  It is also a local variable,
		 * declared on the stack, in Task execute() routines,
		 * which shadows that global variable.
		 */
	/* Actual name used to declare this variable */
  public final static String CXX_ENCLOSING_TASK_REF
				=   "_TDL_ENCLOSING_TASK";


	/* Reference for constraints... */
  public final static String CXX_ENCLOSING_TASK_CONSTRAINT_OBJECT
				=   CXX_TDL_TASK_HANDLER_INSTANCE
				  + " . getEnclosingTaskTreeNode()";

		/* The previous-node inside TDL tasks */
  public final static String CXX_TDL_PREVIOUS_INTERNAL_TASK_REF
				=   "_TDL_Constraint::PREVIOUS";

		/* The previous-node (last-child) in TCM outside of tasks */
   /* Note: Changed in restorePreviousRootNodeValueForBackwardCompatibility()*/
  public       static String CXX_TCM_PREVIOUS_EXTERNAL_TASK_REF
				=   "TCM_LastChild ( "
				  + DataComponent.CXX_TCM_ROOT_NODE
				  + " )";

		/* The CHILD-node reference for constraints. */
  public final static String CXX_TDL_CONSTRAINT_CHILD_TASK_REF
				=   "_TDL_Constraint::SELF";




		/* TDL Constraint Functions/Classes: */ 

  public final static String CXX_TDL_EXPAND_FIRST
			     = "_TDL_DO_TCM_EXPAND_FIRST";
  public final static String CXX_TDL_EXPAND_FIRST_MONITOR_TASK_LEVEL
			     = "_TDL_DO_TCM_EXPAND_FIRST_MONITOR_TASK_LEVEL";
  public final static String CXX_TDL_EXPAND_FIRST_CLASS
			     = "_TDL_ExpandFirst";

  public final static String CXX_TDL_DELAY_EXPANSION
			    = "_TDL_DO_TCM_DELAY_EXPANSION";
  public final static String CXX_TDL_DELAY_EXPANSION_MONITOR_TASK_LEVEL
			    = "_TDL_DO_TCM_DELAY_EXPANSION_MONITOR_TASK_LEVEL";
  public final static String CXX_TDL_DELAY_EXPANSION_CLASS
			    = "_TDL_DelayExpansion";

  public final static String CXX_TDL_SEQUENTIAL_HANDLING
			     = "_TDL_DO_TCM_SEQUENTIAL_HANDLING";
	/* There currently is no MONITOR-Task-Level SEQUENTIAL_HANDLING... */
  public final static String CXX_TDL_SEQUENTIAL_HANDLING_CLASS
			     = "_TDL_SequentialHandling";

  public final static String CXX_TDL_SEQUENTIAL_EXPANSION
		       = "_TDL_DO_TCM_SEQUENTIAL_EXPANSION";
  public final static String CXX_TDL_SEQUENTIAL_EXPANSION_MONITOR_TASK_LEVEL
		       = "_TDL_DO_TCM_SEQUENTIAL_EXPANSION_MONITOR_TASK_LEVEL";
  public final static String CXX_TDL_SEQUENTIAL_EXPANSION_CLASS
		       = "_TDL_SequentialExpansion";

  public final static String CXX_TDL_SEQUENTIAL_EXECUTION
		       = "_TDL_DO_TCM_SEQUENTIAL_EXECUTION";
  public final static String CXX_TDL_SEQUENTIAL_EXECUTION_MONITOR_TASK_LEVEL
		       = "_TDL_DO_TCM_SEQUENTIAL_EXECUTION_MONITOR_TASK_LEVEL";
  public final static String CXX_TDL_SEQUENTIAL_EXECUTION_CLASS
		       = "_TDL_SequentialExecution";

  public final static String CXX_TDL_SERIAL
			     = "_TDL_DO_TCM_SERIAL";
  public final static String CXX_TDL_SERIAL_MONITOR_TASK_LEVEL
			     = "_TDL_DO_TCM_SERIAL_MONITOR_TASK_LEVEL";
  public final static String CXX_TDL_SERIAL_CLASS
			     = "_TDL_Serial";

  public final static String CXX_TDL_PARALLEL
			     = "_TDL_DO_TCM_PARALLEL";
  public final static String CXX_TDL_PARALLEL_CLASS
			     = "_TDL_Parallel";

  public final static String CXX_TDL_WAIT
			     = "_TDL_DO_TCM_WAIT";
  public final static String CXX_TDL_WAIT_CLASS
			     = "_TDL_Wait";

  public final static String CXX_TDL_DISABLE_UNTIL_EVENT
			     = "_TDL_DO_TCM_DISABLE_UNTIL_EVENT";
  public final static String CXX_TDL_DISABLE_UNTIL_EVENT_CLASS
			     = "_TDL_DisableUntilEvent";

  public final static String CXX_TDL_DISABLE_UNTIL
			     = "_TDL_DO_TCM_DISABLE_UNTIL";
  public final static String CXX_TDL_DISABLE_UNTIL_CLASS
			     = "_TDL_DisableUntilTime";

  public final static String CXX_TDL_DISABLE_FOR
			     = "_TDL_DO_TCM_DISABLE_FOR";
  public final static String CXX_TDL_DISABLE_FOR_CLASS
			     = "_TDL_DisableForTime";

  public final static String CXX_TDL_TERMINATE_AT_EVENT
			     = "_TDL_DO_TCM_TERMINATE_AT_EVENT";
  public final static String CXX_TDL_TERMINATE_AT_EVENT_CLASS
			     = "_TDL_TerminateAtEvent";

  public final static String CXX_TDL_TERMINATE_AT
			     = "_TDL_DO_TCM_TERMINATE_AT";
  public final static String CXX_TDL_TERMINATE_AT_CLASS
			     = "_TDL_TerminateAtTime";

  public final static String CXX_TDL_TERMINATE_IN
			     = "_TDL_DO_TCM_TERMINATE_IN";
  public final static String CXX_TDL_TERMINATE_IN_CLASS
			     = "_TDL_TerminateInTime";

  public final static String CXX_TDL_TERMINATE_IMMEDIATE
			     = "_TDL_DO_TCM_TERMINATE_IMMEDIATE";
  public final static String CXX_TDL_TERMINATE_IMMEDIATE_CLASS
			     = "_TDL_TerminateImmediate";

  public final static String CXX_TDL_ACTIVATE_AT_EVENT
			     = "_TDL_DO_TCM_ACTIVATE_AT_EVENT";
  public final static String CXX_TDL_ACTIVATE_AT_EVENT_CLASS
			     = "_TDL_ActivateAtEvent";

  public final static String CXX_TDL_ACTIVATE_AT
			     = "_TDL_DO_TCM_ACTIVATE_AT";
  public final static String CXX_TDL_ACTIVATE_AT_CLASS
			     = "_TDL_ActivateAtTime";

  public final static String CXX_TDL_ACTIVATE_IN
			     = "_TDL_DO_TCM_ACTIVATE_IN";
  public final static String CXX_TDL_ACTIVATE_IN_CLASS
			     = "_TDL_ActivateInTime";

  public final static String CXX_TDL_ACTIVATE_IMMEDIATE
			     = "_TDL_DO_TCM_ACTIVATE_IMMEDIATE";
  public final static String CXX_TDL_ACTIVATE_IMMEDIATE_CLASS
			     = "_TDL_ActivateImmediate";

  public final static String CXX_TDL_SET_MAXIMUM_ACTIVATE
			     = "_TDL_DO_SET_MONITOR_MAXIMUM_ACTIVATIONS";
  public final static String CXX_TDL_SET_MAXIMUM_ACTIVATE_CLASS
			     = "_TDL_SetMonitorMaximumActivations";

  public final static String CXX_TDL_SET_MAXIMUM_TRIGGER
			     = "_TDL_DO_SET_MONITOR_MAXIMUM_TRIGGERS";
  public final static String CXX_TDL_SET_MAXIMUM_TRIGGER_CLASS
			     = "_TDL_SetMonitorMaximumTriggers";

  public final static String CXX_TDL_SET_MONITOR_PERIOD
			     = "_TDL_DO_SET_MONITOR_PERIOD";
  public final static String CXX_TDL_SET_MONITOR_PERIOD_CLASS
			     = "_TDL_SetMonitorPeriod";

  public final static String CXX_TDL_ADD_EXCEPTION_HANDLER
			     = "_TDL_DO_ADD_EXCEPTION_HANDLER";
  public final static String CXX_TDL_ADD_EXCEPTION_HANDLER_CLASS
			     = "_TDL_AddExceptionHandler";

  public final static String CXX_TDL_ON_TERMINATE
			     = "_TDL_DO_TCM_ON_TERMINATION";
  public final static String CXX_TDL_ON_TERMINATE_CLASS
			     = "_TDL_OnTermination";

  public final static String CXX_TDL_ON_AGENT_CLASS
			     = "_TDL_OnAgent";



		/* allocate function return value */
  public final static String CXX_ALLOCATE_RETURN_VALUE
				=   "TCM_Task_Tree_Ref";

		/* Allocate function name (lead) */
  public final static String CXX_ALLOCATE_FUNCTION_LEAD
				= "_TDL_Allocate_";

		/* Allocate function name argument type */
  public final static String CXX_ALLOCATE_ARGUMENT_TYPE
				=   "STRING";

		/* Allocate function name argument name */
  public final static String CXX_ALLOCATE_ARGUMENT_NAME
				=   "theName";

		/* Allocate function agent name argument type */
  public final static String CXX_ALLOCATE_AGENT_ARGUMENT_TYPE
				=   "STRING";

		/* Allocate function agent name argument name */
  public final static String CXX_ALLOCATE_AGENT_ARGUMENT_NAME
				=   "theAgentName";

		/* Allocate function is distributed remote argument type */
  public final static String CXX_ALLOCATE_IS_DISTRIBUTED_REMOTE_ARGUMENT_TYPE
				=   "BOOLEAN";

		/* Allocate function is distributed remote argument name */
  public final static String CXX_ALLOCATE_IS_DISTRIBUTED_REMOTE_ARGUMENT_NAME
				=   "theIsDistributedRemoteSide";

		/* Allocate variable name */
  public final static String CXX_ALLOCATED_NODE_NAME
				=   "allocatedNode";


		/* Creator function name (lead) */
  public final static String CXX_CREATOR_FUNCTION_LEAD
				= "_TDL_Exception_Creator_";

		/* Creator function first argument type */
  public final static String CXX_CREATOR_ARG_EXCEP_TYPE
				=   "STRING";

		/* Creator function first argument name */
  public final static String CXX_CREATOR_ARG_EXCEP_NAME
				=   "theName";

		/* Creator function second argument type */
  public final static String CXX_CREATOR_ARG_DATA_TYPE
				=   "const void *";

		/* Creator function second argument name */
  public final static String CXX_CREATOR_ARG_DATA_NAME
				=   "theData";

		/* Creator function internal argument type */
  public final static String CXX_CREATOR_EXCEP_DATA_TYPE
				=   "struct exceptionData";

		/* Creator function internal argument name */
  public final static String CXX_CREATOR_EXCEP_DATA_NAME
				=   "exceptionData";


		/* createAction function return value */
  public final static String CXX_CREATEACTION_RETURN_VALUE
				= "_TDL_ActionOrVoid";

		/* createAction function name (lead) */
  public final static String CXX_CREATEACTION_FUNCTION_LEAD
				= "_TDL_CreateAction_";

		/* createAction Task-Ref argument-type */
  public final static String CXX_CREATEACTION_TASK_ARGUMENT_TYPE
				= DataComponent.CXX_ALLOCATE_RETURN_VALUE
				  + " &";
		/* createAction Task-Ref argument */
  public final static String CXX_CREATEACTION_TASK_ARGUMENT
				= DataComponent.CXX_ENCLOSING_TASK_REF;

		/* createAction variable name */
  public final static String CXX_CREATEACTION_VARIABLE_NAME
				= "createdActionOrVoid";

		/* create-distributed-remote-Action function return value */
		/* Note: Signature Hardcoded/Specified in TCM!            */
  public final static String CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_RETURN_VALUE
				= "TCM_Action_Ref";

		/* create-distributed-remote-Action function name (lead) */
  public final static String CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_FUNCTION_LEAD
			       ="_TDL_UnpackAndCreateDistributedRemoteAction_";

		/* create-distributed-remote-Action void argument */
  public final static String CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_VOID_ARGUMENT
				= "theVoidPointerArgumentToUnpack";

		/* create-distributed-remote-Action JUNK argument */
  public final static String CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_JUNK_ARGUMENT
				= "theTcmJunkClientData";

		/* create-distributed-remote-Action function arguments, *
		 * name & type.                                         *
		 * Note: Signature Hardcoded/Specified in TCM!          */
  public final static String[]
			CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_FUNCTION_ARGUMENTS
    = { "TCM_Task_Tree_Ref & "
	  + DataComponent.CXX_ENCLOSING_TASK_REF,
	"void *              "
	  + DataComponent.CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_VOID_ARGUMENT,
	"void *              "
	  + DataComponent.CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_JUNK_ARGUMENT
      };

		/* create-distributed-remote-Action function argument  *
		 * names to mark as "used" to avoid compiler warnings. */
  public final static String[]
		 CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_FUNCTION_ARGUMENT_NAMES
    = { DataComponent.CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_VOID_ARGUMENT,
	DataComponent.CXX_CREATE_DISTRIBUTED_REMOTE_ACTION_JUNK_ARGUMENT
      };

		/* Distributed (void*) struct name */
  public final static String CXX_DISTRIBUTED_STRUCT_NAME
				=   "_TDL_DISTRIBUTED_STRUCT";

		/* Distributed (void*) struct pointer */
  public final static String CXX_DISTRIBUTED_STRUCT_POINTER
				=   "_TDL_distributedStructPointer";

		/* Distributed _TDL_ActionOrVoid (_TDL_Action *) set method */
  public final static String CXX_CREATE_ACTION_OR_VOID_SET_ACTION
				=   "setActionPointer";

		/* Distributed _TDL_ActionOrVoid (void*) set method */
  public final static String CXX_CREATE_ACTION_OR_VOID_SET_VOID
				=   "setVoidPointer";



		/* constant (optional) maximum activations argument-type */
  public final static String CXX_CONSTANT_MAXIMUM_ACTIVATIONS_ARGUMENT_TYPE
				= "u_int4";
		/* constant (optional) maximum activations argument */
  public final static String CXX_CONSTANT_MAXIMUM_ACTIVATIONS_ARGUMENT
				= "_TDL_MaximumActivates";
		/* constant (optional) maximum activations default value */
  public final static String CXX_CONSTANT_MAXIMUM_ACTIVATIONS_DEFAULT_VALUE
				= DataComponent.CXX_TCM_DEFAULT_MAX_ACTIVATES;

		/* RTTI methods */
  public final static String CXX_RTTI_METHODS_PART_ONE
				=   "static  STRING TCM_getStaticName() { return \"TDL-";
  public final static String CXX_RTTI_METHODS_PART_TWO
				=   "\"; }\nvirtual STRING TCM_getActionName() { return TCM_getStaticName(); }";

		/* Distributed Registry attribute type */
  public final static String CXX_DISTRIBUTED_REGISTRY_TYPE
				=   "_TDL_DistributedRegistryEntry";

		/* Distributed Exception Registry attribute type */
  public final static String CXX_DISTRIBUTED_EXCEPTION_REGISTRY_TYPE
				=   "_TDL_DistributedExceptionRegistryEntry";

		/* Distributed Registry attribute name */
  public final static String CXX_DISTRIBUTED_REGISTRY_NAME
				=   "_TDL_thisDistributedRegistryEntry";

		/* Distributed Registry overloaded-task-name-index method */
 public final static String CXX_DISTRIBUTED_REGISTRY_OVERLOADED_TASK_NAME_INDEX
				=   "getOverloadedTaskNameIndex";


		/* Verify Node-Class-Type declaration */
  public final static String CXX_VERIFY_NODE_CLASS_TYPE
				=   "TDL::verifyNodeClassType";
  public final static String CXX_TCM_GOAL    = "TCM_Goal";
  public final static String CXX_TCM_COMMAND = "TCM_Command";
  public final static String CXX_TCM_MONITOR = "TCM_Monitor";


		/* Constants to represent the various allocation strategies. */
  public final static int    LOCAL_NONDISTRIBUTED_ONLY   = 1;
  public final static int    EITHER_LOCAL_OR_DISTRIBUTED = 2;
  public final static int    DISTRIBUTED_ONLY            = 3;


		/* Verify Node Allocation-Function Type declarations */
  public final static String CXX_TDL_CACHE_NODE_ALLOCATION_FUNCTION_TYPE
		=   "_TDL_AllocationFunction::cacheAllocationFunctionType";

 public final static String CXX_TDL_ALLOCATION_TYPE_LOCAL_NONDISTRIBUTED_ONLY
		=   "_TDL_AllocationFunction::LOCAL_NONDISTRIBUTED_ONLY";
 public final static String CXX_TDL_ALLOCATION_TYPE_EITHER_LOCAL_OR_DISTRIBUTED
		=   "_TDL_AllocationFunction::EITHER_LOCAL_OR_DISTRIBUTED";
 public final static String CXX_TDL_ALLOCATION_TYPE_DISTRIBUTED_ONLY
		=   "_TDL_AllocationFunction::DISTRIBUTED_ONLY";
 public final static String CXX_TDL_ALLOCATION_TYPE_UNKNOWN
		=   "_TDL_AllocationFunction::UNKNOWN";

  public final static String CXX_VERIFY_NODE_ALLOCATION_FUNCTION
				=   "TDL::verifyNodeAllocationFunction";


		/* Delayed Task Allocation with static signature checking. */
  public final static String CXX_LOCAL_NONDISTRIBUTED_ONLY_DELAYED_ALLOCATION
		=   "const _TDL_DelayedAllocation_Local &";
  public final static String CXX_EITHER_LOCAL_OR_DISTRIBUTED_DELAYED_ALLOCATION
		=   "const _TDL_DelayedAllocation_Either &";
  public final static String CXX_DISTRIBUTED_ONLY_DELAYED_ALLOCATION
		=   "const _TDL_DelayedAllocation_Distributed &";

  public final static String CXX_INVOKE_AS_LOCAL_NONDISTRIBUTED_ONLY_DELAYED_ALLOCATION
		=   "_TDL_InvokeDelayedAllocation_Local";
  public final static String CXX_INVOKE_AS_EITHER_LOCAL_OR_DISTRIBUTED_DELAYED_ALLOCATION
		=   "_TDL_InvokeDelayedAllocation_Either";
  public final static String CXX_INVOKE_AS_DISTRIBUTED_ONLY_DELAYED_ALLOCATION
		=   "_TDL_InvokeDelayedAllocation_Distributed";


  public final static String CXX_DELAYED_ALLOCATION_OBJECT
		=   "_TDL_DelayedAllocationObject";

  public final static String CXX_TDL_PROCESS_DELAYED_ALLOCATION_CAN_ABORT
		=   "_TDL_ProcessDelayedAllocation_CanAbort";
  public final static String CXX_DELAYED_ALLOCATION_GET_NAME_METHOD
		=   "getTaskName()";
  public final static String CXX_DELAYED_ALLOCATION_GET_AGENT_NAME_METHOD
		=   "getTaskAgentName()";
  public final static String CXX_DELAYED_ALLOCATION_IS_NOT_DISTRIBUTED_REMOTE_SIDE
		=   "FALSE";






		/* Execute method return value. */
  public final static String CXX_EXECUTE_RETURN_VALUE
				=   "void";

		/* Execute method signature. */
  public final static String CXX_EXECUTE_METHOD 
				=   "execute ( const TCM_Task_Tree_Ref & "
                                 + DataComponent.CXX_ENCLOSING_TASK_REF + " )";

		/* Resume method return value. */
  public final static String CXX_RESUME_RETURN_VALUE
				=   "void";

		/* Resume method signature. */
  public final static String CXX_RESUME_NAME
				=   "resume";

	    /* TCM_PushUserTaskForThisThreadForDurationOfObject declaration */
  public final static String CXX_PUSH_USER_TASK_FOR_STACK_FRAME_DECLARATION
	       =   "TCM_PushUserTaskForThisThreadForDurationOfObject\n"
                 + "  _TDL_PushOurTaskForThisThreadForTheDurationOfThisObject";

		/* Resume _TDL_ENCLOSING_TASK declaration */
  public final static String CXX_RESUME_TDL_ENCLOSING_TASK_DECLARATION
				=   "\nTCM_Task_Tree_Ref "
				  + DataComponent.CXX_ENCLOSING_TASK_REF
				  + " = _TDL_getTcmTaskTreeNode();\n"
				  + "_TDL_clearTcmTaskTreeNode();\n"
		 + DataComponent.CXX_PUSH_USER_TASK_FOR_STACK_FRAME_DECLARATION
				  + " ( "
                                  + DataComponent.CXX_ENCLOSING_TASK_REF
				  + " );\n";


		/* Static Resume method return value. */
  public final static String CXX_RESUME_FUNCTION_RETURN_VALUE
				=   "void";

		/* Static Resume method return value. */
  public final static String CXX_RESUME_FUNCTION_LEAD
				=   "TDL_RESUME_";

		/* Static Resume method's first argument */
  public final static String CXX_RESUME_FUNCTION_ARGUMENT
				=   "const TCM_Task_Tree_Ref & "
				  + DataComponent.CXX_ENCLOSING_TASK_REF;


		/* spawnAndWait function return value */
  public final static String CXX_SPAWNWAIT_RETURN_VALUE	
				=   "TCM_Return_Type";

		/* spawnAndWait function name (lead) */
  public final static String CXX_SPAWNWAIT_FUNCTION_LEAD
				= "_TDL_SpawnAndWait_";

		/* spawnAndWait function ref argument type */
  public final static String CXX_SPAWNWAIT_ARGUMENT_TYPE
				=   "const TCM_Task_Tree_Ref &";

		/* spawnAndWait function ref argument name */
  public final static String CXX_SPAWNWAIT_ARGUMENT_NAME
				=   "theParentTaskTreeRef";

		/* spawnAndWait function return variable */
  public final static String CXX_SPAWNWAIT_RETURN_VARIABLE	
				=   "returnValue";

		/* spawnAndWait child-node name */
  public final static String CXX_SPAWNWAIT_CHILD_NODE_NAME
				=   "spawnChild";


		/* Exception Data method return value. */
  public final static String CXX_GET_EXCEPTION_DATA_RETURN_VALUE
				=   "const void *";

		/* Exception Data method signature. */
  public final static String CXX_GET_EXCEPTION_DATA_METHOD
				=   "getExceptionData";

		/* Exception Name method return value. */
  public final static String CXX_STATIC_EXCEPTION_NAME_RETURN_VALUE
				=   "STRING";

		/* Exception Name method signature. */
  public final static String CXX_STATIC_EXCEPTION_NAME_METHOD
				=   "getStaticExceptionName";

		/* Exception Name method return value. */
  public final static String CXX_EXCEPTION_NAME_RETURN_VALUE
				=   "STRING";

		/* Exception Name method signature. */
  public final static String CXX_EXCEPTION_NAME_METHOD
				=   "getExceptionName() const";


		/* Exception Matches method name. */
  public final static String CXX_EXCEPTION_MATCHES_METHOD_NAME
				=   "matches";

		/* Exception clone method name. */
  public final static String CXX_EXCEPTION_CLONE_METHOD_NAME
				=   "clone";

		/* Exception Matches method argument. */
  public final static String CXX_EXCEPTION_MATCHES_ARGUMENT
				=   "theString";

		/* Exception Matches method return value. */
  public final static String CXX_EXCEPTION_MATCHES_RETURN_VALUE
				= "BOOLEAN";

		/* Exception Matches method signature. */
  public final static String CXX_EXCEPTION_MATCHES_METHOD
			   =   DataComponent.CXX_EXCEPTION_MATCHES_METHOD_NAME
                             + " ( STRING "
                             + DataComponent.CXX_EXCEPTION_MATCHES_ARGUMENT
                             + " ) const";

		/* Exception clone method return value. */
  public final static String CXX_EXCEPTION_CLONE_RETURN_VALUE
			     = DataComponent.CXX_CREATE_EXCEPTION_RETURN_VALUE;

		/* Exception clone method signature. */
  public final static String CXX_EXCEPTION_CLONE_METHOD
			   =   DataComponent.CXX_EXCEPTION_CLONE_METHOD_NAME
                             + " ( void ) const";


		/* create Exception function return value */
  public final static String CXX_CREATE_EXCEPTION_RETURN_VALUE
				= DataComponent.CXX_TCM_EXCEPTION_CLASS
				  + " *";

		/* create Exception function name (lead) */
  public final static String CXX_CREATE_EXCEPTION_FUNCTION_LEAD
				= "_TDL_CreateException_";


		/* Exception-Handler handled-exception-name return value. */
  public final static String CXX_HANDLED_EXCEPTION_NAME_RETURN_VALUE
				=   "STRING";

		/* Exception-Handler handled-exception-name method signature.*/
  public final static String CXX_HANDLED_EXCEPTION_NAME_METHOD
				=   "_TDL_getHandledExceptionName() const";



		/* create Exception Handler function return value */
  public final static String CXX_CREATE_HANDLER_RETURN_VALUE
				= DataComponent.CXX_HANDLER_BASE_CLASS
				  + " *";

		/* create Exception Handler function name (lead) */
  public final static String CXX_CREATE_HANDLER_FUNCTION_LEAD
				= "_TDL_CreateExceptionHandler_";


		/* means to access FAIL data inside an Exception Handler */
  public final static String CXX_HANDLER_EXCEPTION_DATA_ACCESS
			 =   "TCM_FailureException ( "
			   + CXX_ENCLOSING_TASK_REF + " )";

		/* Used when indexing constraints becomes necessary. */
  public final static int    UNKNOWN_CONSTRAINT_INDEX = -1;




		/* Good location for standard constants for subclasses. */
  protected static int    STANDARD_INDENT   = 2;
  protected static int    STANDARD_TAB_SIZE = 8;




	/* Class Methods */
  public static boolean isEmptyString ( String theString )
  {
    return (   ( theString            == null )
	    || ( theString . length() <= 0    ) );
  }

  public static int    getIndent()          { return STANDARD_INDENT;   }
  public static int    getTabSize()         { return STANDARD_TAB_SIZE; }

  public static String getCxxSubsetName ( int theCxxSubset )
  {
    if (   ( theCxxSubset >= DataComponent.CXX_HEADER                     )
	&& ( theCxxSubset <= DataComponent.CXX_CODE_AND_HEADER_SEQUENTIAL ) )
    {
      return
	DataComponent.CXX_STRINGS [ theCxxSubset - DataComponent.CXX_HEADER ];
    }
     /* Lets just include HTML documentation subset-type for the heck of it. */
    else if ( theCxxSubset == DataComponent.HTML_DOCUMENTATION )
    {
      return "HTML_DOCUMENTATION";
    }
    else
      return ( "ERROR-UNKNOWN-CXX_SUBSET(" + theCxxSubset + ")" );
  }

	/* This is a cheap down & dirty mechanism.  But it's fast! */
  public static String spaceString
    = "                                                                      ";
  public static String getSpaceString ( int theLength )
  {
    if ( theLength <= DataComponent.spaceString . length() )
      return DataComponent.spaceString . substring ( 0, theLength );
    else
      return DataComponent.spaceString
	+ getSpaceString ( theLength - DataComponent.spaceString . length() );
  }

  public static boolean isValidLineNumber ( int theLineNumber )
  {
    return theLineNumber != DataComponent.INVALID_LINE_NUMBER;
  }


       /** Mechism for sorting through all the various Exceptions/Errors
	 *  that the TLDParser might throw...
	 * Note:  Made static to permit access from certain static methods.
	 * Originally located in DataComponent back in the good old GUI "VDT"
	 * days, when we needed to be able to re-parse subclasses...
	 * Now, it remains, since it is used by parsing operations elsewhere.
	 */
  public static void didParseOfSubpartFail ( Throwable theExceptionOrError )
    throws DetailedParseException
  {
      if ( theExceptionOrError instanceof ParseException )
	throw new DetailedParseException ( (ParseException)
					   theExceptionOrError );

      if ( theExceptionOrError instanceof TokenMgrError )
      {
	System.err.println (
	    "[DataTaskDefinition:didParseOfSubpartFail]  Warning:  "
	    + "Parsing returned a TokenMgrError...  Failing..." );
	throw new DetailedParseException ( (TokenMgrError)
					   theExceptionOrError );
      }

      if ( theExceptionOrError instanceof DetailedParseException )
	throw (DetailedParseException) theExceptionOrError;

      if ( theExceptionOrError instanceof Error )
	throw (Error) theExceptionOrError;

      if ( theExceptionOrError instanceof RuntimeException )
	throw (RuntimeException) theExceptionOrError;

      System.err.println (
	  "[DataTaskDefinition:didParseOfSubpartFail]  Warning:  Exception \""
	  + theExceptionOrError . getClass() . getName()
	  + "\" was not anticipated..." );

      throw new DetailedParseException ( theExceptionOrError );
  }





	/* Instance Data */
  protected DataComponent  parentContainer;
  protected DataVector     subcomponents;
  protected DataHashtable  indexData;
  protected int            generateSubcomponentsIndex;
  protected int            lineNumber;



	/* Instance Methods */
  public DataComponent ( )
  {
    parentContainer = null;
    subcomponents   = new DataVector();
    indexData       = new DataHashtable();
    initializeGenerateSubcomponentIndex();
    lineNumber      = DataComponent.INVALID_LINE_NUMBER;
  }


  public DataComponent getParent ( ) { return parentContainer; }
  public void          setParent ( DataComponent theParentContainer )
				     { parentContainer = theParentContainer; }

  public String getMessageFilenameLead()
  {
    if ( getParent() != null )
      return getParent() . getMessageFilenameLead();
    else
    {
      System.err.println ( "[DataComponent:getMessageFilenameLead]  Error: "
			 + "There is no parent -- Unable to get filename for "
			 + "object of class \"" + getClass().getName()
			 + "\".  Using default of \"Line\".  "
			 + "PLEASE REPORT THIS ERROR!" );
      return DataComponent.DEFAULT_MESSAGE_FILENAME_LEAD;
    }
  }


  public DataTaskDefinition getParentTaskDefinition ()
  {
    for ( DataComponent component = this;
	  component != null;
	  component  = component . getParent() )
    {
      if ( component instanceof DataTaskDefinition )
      {
	return (DataTaskDefinition) component;
      }
    }
    return null;
  }

  public String getUniqueIdentifierString ( )
  {
    return getUniqueIdentifierString ( (String) null );
  }

  public String getUniqueIdentifierString ( String theLeadString )
  {
    StringBuffer  uniqueIdentifierStringBuffer = new StringBuffer();

    if ( theLeadString != null )
      uniqueIdentifierStringBuffer . append ( theLeadString );
    else
      uniqueIdentifierStringBuffer . append ( getMessageFilenameLead() );

	/* Hashcode + time in milliseonds should make us pretty unique. */

    uniqueIdentifierStringBuffer . append ( "0x" );
    uniqueIdentifierStringBuffer
      . append ( Long.toHexString ( System.currentTimeMillis() ) );

    uniqueIdentifierStringBuffer . append ( "_0x" );
    uniqueIdentifierStringBuffer
      . append ( Integer.toHexString ( hashCode() ) );

    return getUniqueIdentifierString ( uniqueIdentifierStringBuffer );
  }

  public String getUniqueIdentifierString (
				 StringBuffer theUniqueIdentifierStringBuffer )
  {
	/* Replace invalid characters with underscores -- like the ':' chars.*/
    for ( int i=0;  i < theUniqueIdentifierStringBuffer.length();  i++ )
    {
      if ( /*NOT*/  !
           (   (   ( theUniqueIdentifierStringBuffer . charAt ( i ) >= 'a' )
		&& ( theUniqueIdentifierStringBuffer . charAt ( i ) <= 'z' ) )

	    || (   ( theUniqueIdentifierStringBuffer . charAt ( i ) >= 'A' )
		&& ( theUniqueIdentifierStringBuffer . charAt ( i ) <= 'Z' ) )

	    || (   ( theUniqueIdentifierStringBuffer . charAt ( i ) >= '0' )
		&& ( theUniqueIdentifierStringBuffer . charAt ( i ) <= '9' ) )

	    || (     theUniqueIdentifierStringBuffer . charAt ( i ) == '_' )
	    )
	  )
      {
	theUniqueIdentifierStringBuffer . setCharAt ( i, '_' );
      }
    } /* for ( int i=0;  i < theUniqueIdentifierStringBuffer.length();  i++ )*/

    return theUniqueIdentifierStringBuffer.toString();
  }





  public boolean addPrimaryChild ( DataComponent theChildToAdd,
				   DataComponent theAddChildAfterThisComponent)
  {
    System.err.println ( "[DataComponent:addPrimaryChild]  Error:  Class \""
			 + getClass().getName()
		     + "\" Does not support addPrimaryChild().  Aborting..." );
    return false;
  }

  public boolean addSecondaryChild (
				  DataComponent theChildToAdd,
				  DataComponent theAddChildAfterThisComponent )
  {
    System.err.println ( "[DataComponent:addSecondaryChild]  Error:  Class \""
			 + getClass().getName()
		   + "\" Does not support addSecondaryChild().  Aborting..." );
    return false;
  }

  public boolean removeChild ( DataComponent theChildToRemove )
  {
    System.err.println ( "[DataComponent:removeChild]  Error:  Class \""
			 + getClass().getName()
			 + "\" Does not support removeChild().  Aborting..." );
    return false;
  }



  protected DataVector getSubcomponents ( )  { return subcomponents; }
  public    int        getSubcomponentsCount ( )
				       { return getSubcomponents() . count(); }

  public Object getSubcomponent ( int theIndex )
  {
    if ( ( theIndex >= 0 )  &&  ( theIndex < getSubcomponentsCount() ) )
      return getSubcomponents() . elementAt ( theIndex );
    else
    {
      System.err.println ( "[DataComponent:getSubcomponent]  Warning:  "
			   + "Illegal index (" + theIndex + ")." );
      return null;
    }
  }

  public boolean isSubcomponentAString ( int theIndex )
	{ return  ( getSubcomponent ( theIndex ) instanceof String ); }

  public boolean isSubcomponentADataComponent ( int theIndex )
	{ return  ( getSubcomponent ( theIndex ) instanceof DataComponent ); }

  public String getStringSubcomponent ( int theIndex )
  {
    if ( isSubcomponentAString ( theIndex ) )
      return (String) getSubcomponent ( theIndex );
    else
    {
      System.err.println (
		 "[DataComponent:getStringSubcomponent]  Warning:  "
		 + "Subcomponent ( " + theIndex
		 + " ) is NOT a String!  It is a \""
		 + getSubcomponent ( theIndex ) . getClass() . getName()
		 + "\"." );
      return null;
    }
  }

  public DataComponent getDataComponentSubcomponent ( int theIndex )
  {
    if ( isSubcomponentADataComponent ( theIndex ) )
      return (DataComponent) getSubcomponent ( theIndex );
    else
    {
      System.err.println (
		 "[DataComponent:getDataComponentSubcomponent]  Warning:  "
		 + "Subcomponent ( " + theIndex
		 + " ) is NOT a DataComponent!  It is a \""
		 + getSubcomponent ( theIndex ) . getClass() . getName()
		 + "\"." );
      return null;
    }
  }

  public int getIndexOfSubcomponent ( Object theObject )
  {
    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
      if ( theObject . equals ( getSubcomponent ( i ) ) )
	return i;
    }
    return DataComponent.INVALID_INDEX;
  }

  public boolean hasSubcomponent ( Object theObject )
  {
    return getIndexOfSubcomponent ( theObject ) != DataComponent.INVALID_INDEX;
  }

  public int getIndexOfSubcomponentFraction ( String theString )
  {
    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
      if (     isSubcomponentAString ( i )
	  && ( getStringSubcomponent ( i ) . indexOf ( theString ) != -1 ) )
	return i;
    }
    return DataComponent.INVALID_INDEX;
  }

	/* Overriden in subclasses.  Searches contained expressions too. */
  public boolean hasSubcomponentFraction ( String theString )
  {
    return (     getIndexOfSubcomponentFraction ( theString )
	     !=  DataComponent.INVALID_INDEX                  );
  }

	/* Overriden in subclasses.  Searches contained expressions too.
	 *
	 * Notes:  Problem is, we want to find instances of TDL_REF to
	 * process the arguments for distributed task checking.  Maintaining
	 * an index into the subcomponents is problematical, unless we
	 * manage a complicated stack of information regarding where we are
	 * in the search, so either:
	 *  (1) We do O(n^m) doing full searches to find the Nth instance
	 *      each time.
	 *  (2) We create a large flat vector of all the subcomponents.
	 *  (3) We use a callback-interface.
	 * Option (3) minimizes memory-utilization and running-time.
	 */
  public void runOnSubcomponentFraction (
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
    runOnSubcomponentFraction ( theString,         theRunOnSubcomponentObject,
				theArgumentObject, true );
  }


  public void runOnSubcomponentFraction (
		       String                      theString,
		       RunOnSubcomponentInterface  theRunOnSubcomponentObject,
		       Object                      theArgumentObject,
		       boolean                     theSearchDataComponentsToo )
  {
    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
      if ( isSubcomponentAString ( i ) )
	staticRunOnStringFraction ( getStringSubcomponent ( i ),
				    this,
				    i,
				    theString,
				    theRunOnSubcomponentObject,
				    theArgumentObject );

      else if (   ( isSubcomponentADataComponent ( i ) )
	       && ( theSearchDataComponentsToo         ) )
	getDataComponentSubcomponent ( i )
	  . runOnSubcomponentFraction ( theString,
					theRunOnSubcomponentObject,
					theArgumentObject );
    }
  }
	/* Normally, static methods would be up top.  This is an exception. */
  public static void staticRunOnStringFraction (
			String                      theStringToSearch,
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
    DataComponent.staticRunOnStringFraction (  theStringToSearch,
					       null,
					       DataComponent.INVALID_INDEX,
					       theString,
					       theRunOnSubcomponentObject,
					       theArgumentObject );
  }


  public static void staticRunOnStringFraction (
			String                      theStringToSearch,
			DataComponent               theDataComponent,
			int                         theSubcomponentIndex,
			String                      theString,
			RunOnSubcomponentInterface  theRunOnSubcomponentObject,
			Object                      theArgumentObject )
  {
    int   result, stringToSearchIndex;

	/* Caveat:  Stripping is expensive, and seldom necessary!
	 * So don't do it unless we need to.
	 */
    if ( theStringToSearch . indexOf ( theString ) != -1 )
    {
	/* The problem is, we may be picking up "theString" inside
	 * comments & macros -- that shouldn't be happening..
	 * So, lets strip them out...
	 * Addendum: Also remove "theString" from inside string literals.
	 */
      TDLParser . reinitParser ( theStringToSearch );
      try
      {
	theStringToSearch
	  = TDLParser . getParser() . parseStripFluff( true /*And Strings*/);
      }
      catch ( Throwable  theExceptionOrError )
      {
	System.err.println ( "[DataTaskDefinition:foundSubcomponentMatch]  "
			  + "Error:  Unexpected error encountered while "
			  + "trying to re-parse out fluff (comments/macros):  "
			  + theExceptionOrError.toString() );
	return;
      }

	/* And try it again. */
      result = theStringToSearch . indexOf ( theString );
    }  /* if ( theStringToSearch . indexOf ( theString ) != -1 ) */
    else
    {
      result = -1;
    }


    do
    {
      if ( result != -1 )
      {
	stringToSearchIndex = result;

	theRunOnSubcomponentObject
	  . foundSubcomponentMatch ( theString,
				     theStringToSearch,
				     stringToSearchIndex,
				     theDataComponent,
				     theSubcomponentIndex,
				     theArgumentObject );
	stringToSearchIndex ++;

	if ( stringToSearchIndex < theString.length() )
	  result = theStringToSearch . indexOf ( theString,
						 stringToSearchIndex );
	else
	  result = -1;
      } /* if ( result != -1 ) */
    } while ( result != -1 );
  }



  public  boolean addSubcomponent ( String theString )
  {
    return privateAddSubcomponent ( theString );
  }
  public  boolean addSubcomponent ( DataComponent theDataComponent )
  {
    return privateAddSubcomponent ( theDataComponent );
  }

  protected boolean privateAddSubcomponent ( Object theObject )
  {
    getSubcomponents() . addElement ( theObject );
    if ( theObject instanceof DataComponent )
      ((DataComponent) theObject) . setParent ( this );
    return true;
  }

  public boolean addSubcomponent ( String theString,  int theLocation )
	   { return privateAddSubcomponent ( theString, theLocation ); }

  public boolean addSubcomponent ( DataComponent theDataComponent,
				   int           theLocation )
	   { return privateAddSubcomponent ( theDataComponent, theLocation ); }

  protected boolean privateAddSubcomponent ( Object theObject, int theLocation)
  {
    if ( theLocation < 0 )
      theLocation = 0;

    if ( theLocation < getSubcomponentsCount() )
      getSubcomponents() . insertElementAt ( theObject, theLocation );
    else
      getSubcomponents() . addElement ( theObject );

    if ( theObject instanceof DataComponent )
      ((DataComponent) theObject) . setParent ( this );

    return true;
  }


  public  boolean removeSubcomponent ( String theString )
  {
    return privateRemoveSubcomponent ( theString );
  }

  public  boolean removeSubcomponent ( DataComponent theDataComponent )
  {
    boolean  returnValue = privateRemoveSubcomponent ( theDataComponent );
    if ( returnValue )
      theDataComponent . setParent ( null );
    return returnValue;
  }

  private boolean privateRemoveSubcomponent ( Object theObject )
  {
    int  index = getIndexOfSubcomponent ( theObject );

    if ( index != DataComponent.INVALID_INDEX )
      return removeSubcomponent ( index );
    else
    {
      System.err.println ( "[DataComponent:privateRemoveSubcomponent]  Error: "
			   + " theObject (\""
			   + theObject . getClass() . getName()
			   + "\") was not found." );
      return false;
    }
  }

  public boolean removeSubcomponent ( int theIndex )
  {
    if ( ( theIndex < 0 )   ||   ( theIndex >= getSubcomponentsCount() ) )
    {
      System.err.println ( "[DataComponent:removeSubcomponent]  Error:  "
			   + "theIndex out of bounds (" + theIndex + ","
			   + 0 + "," + getSubcomponentsCount() + ")" );
      return false;
    }

    getSubcomponents() . removeElementAt ( theIndex );
    return true;
  }
  

  public  boolean replaceSubcomponent ( String theString,  int theIndex )
  {
    return privateReplaceSubcomponent ( theString, theIndex );
  }

  public  boolean replaceSubcomponent ( DataComponent theDataComponent,
					int           theIndex  )
  {
    DataComponent  oldComponent = null;
    boolean        returnValue;

    if ( isSubcomponentADataComponent ( theIndex ) )
      oldComponent = getDataComponentSubcomponent ( theIndex );

    returnValue = privateReplaceSubcomponent ( theDataComponent, theIndex );

    if ( returnValue )
    {
      theDataComponent . setParent ( this );

	/* Only reset parent to null if we replaced (removed) this object... */
      if ( oldComponent != null )
	oldComponent . setParent ( null );
    }

    return returnValue;
  }

  private boolean privateReplaceSubcomponent ( Object theObject, int theIndex )
  {
    if ( theIndex < 0 )
    {
      System.err.println ( "[DataComponent:privateReplaceSubcomponent]  Error:"
			   + "   theIndex < 0." );
      return false;
    }

    if ( theIndex >= getSubcomponentsCount() )
    {
      System.err.println ( "[DataComponent:privateReplaceSubcomponent]  Error:"
			   + "   theIndex >= getSubcomponentsCount(). ["
			   + getSubcomponentsCount() + "]." );
      return false;
    }

    getSubcomponents() . replaceElementAt ( theIndex, theObject );
    return true;
  }


  public boolean hasLeadingWhitespaceSubcomponent()
  {
    int     i, j;
    String  string;

    for ( i=0;  i < getSubcomponentsCount();  i++ )
    {
      if ( isSubcomponentAString ( i ) == false )
	return false;

      string = getStringSubcomponent ( i );

      for ( j=0;  j < string.length();  j++ )
      {
	if ( Character.isWhitespace ( string.charAt ( j ) ) )
	  return true;
	else
	  return false;
      }
    }

    return false;
  }


  public int     getLeadingWhitespaceIndent ( )
				{ return getLeadingWhitespaceIndent ( null ); }

  public int     getLeadingWhitespaceIndent ( String theFirstIndex )
  {
    int     i, j;
    int     column                    = 0;
    int     maximumSubcomponentsCount = getSubcomponentsCount();
    String  string;


    if ( theFirstIndex != null )
    {
	/* Idiocy check */
      if ( hasIndex ( theFirstIndex ) == false )
      {
	System.err.println ( "[DataComponent:getLeadingWhitespaceIndent]  "
		      + "Warning:  Unknown Index \"" + theFirstIndex + "\"" );
      }

	/* Find out how many subcomponets we should search... */
      if ( maximumSubcomponentsCount > getIndex ( theFirstIndex ) )
	maximumSubcomponentsCount = getIndex ( theFirstIndex );
    }


    for ( i=0;  i < maximumSubcomponentsCount;  i++ )
    {
      if ( isSubcomponentAString ( i ) == false )
	return column;

      string = getStringSubcomponent ( i );

      for ( j=0;  j < string.length();  j++ )
      {
	if (   ( string.charAt ( j ) == '\n' )
	    || ( string.charAt ( j ) == '\f' ) )
	{
	  column = 0;
	}

	else if ( string.charAt ( j ) == ' ' )
	{
	  column++;
	}

	else if ( string.charAt ( j ) == '\t' )
	{
	  column += DataComponent.getTabSize()
	           - ( column % DataComponent.getTabSize() );
	}

	else if ( Character.isWhitespace ( string.charAt ( j ) ) == false )
	{
	  return column;
	}
      }
    }

    return column;
  }


  public boolean hasTrailingWhitespaceSubcomponent()
  {
    int     i, j;
    String  string;

    for ( i = getSubcomponentsCount() - 1;   i >= 0;    i-- )
    {
      if ( isSubcomponentAString ( i ) == false )
	return false;

      string = getStringSubcomponent ( i );

      for ( j = string.length() - 1;   j >= 0;  j -- )
      {
	if ( Character.isWhitespace ( string.charAt ( j ) ) )
	  return true;
	else
	  return false;
      }
    }

    return false;
  }



	/** Checks for equivalence based on contained data.
	 *  Note: Parent is NOT checked!
	 */
  public boolean subcomponentsEquals ( Object theObject )
  {
    if (   ( theObject == null )
	|| ( /*NOT*/ !  ( theObject instanceof DataComponent ) ) )
    {
      return false;
    }


    DataComponent  theDataComponent = (DataComponent) theObject;

    if ( getSubcomponentsCount() != theDataComponent.getSubcomponentsCount() )
    {
      return false;
    }

    for ( int i=0;  i < getSubcomponentsCount();  i++ )
    {
      if (   (    isSubcomponentADataComponent ( i )
	       != theDataComponent.isSubcomponentADataComponent ( i ) )

	  || (    isSubcomponentADataComponent ( i )
	       && ( getDataComponentSubcomponent ( i )
		     . subcomponentsEquals (
				     theDataComponent.getSubcomponent ( i ) ) )
	     )
	  || ( getSubcomponent ( i )
	         . equals ( theDataComponent.getSubcomponent ( i ) ) == false )
	  )
      {
	return false;
      }
    }

    return true;
  }


  public void setLineNumber( int theLineNumber ) { lineNumber = theLineNumber;}
  public int  getLineNumber()                    { return lineNumber;         }

  public boolean hasValidLineNumber()
  {
    return DataComponent.isValidLineNumber ( getLineNumber() );
  }

  public String getLineNumberString ( )
  {
    if ( hasValidLineNumber() == false )
      return "??";
    else
      return Integer.toString ( getLineNumber() );
  }







  


	/* These Indexes are (typically) used to reference key points
	 * in the subcomponents vector where non-subcomponents items
	 * should be inserted...  For convenience, this is set up to
	 * return these index numbers based on string-names rather than
	 * numeric indexes into another vector or array...
	 */
  protected DataHashtable getIndexData ( )  { return indexData; }

  public boolean hasIndex ( String theIndexName )
		  { return getIndexData() . get ( theIndexName ) != null; }

  public int getIndex ( String theIndexName )
  {
    if ( theIndexName == null )
      return 0;

    Integer returnValue = (Integer) ( getIndexData() . get ( theIndexName ) );

    if ( returnValue == null )
      return 0;
    else
      return returnValue.intValue();
  }

  public void setIndex ( String theIndexName )
	{ setIndex ( theIndexName, new Integer ( getSubcomponentsCount() ) ); }

  public void setIndex ( String theIndexName, int theIndex )
	{ setIndex ( theIndexName, new Integer ( theIndex ) ); }

  public void setIndex ( String theIndexName, Integer theIndex )
  {
    getIndexData() . put ( theIndexName, theIndex );
  }

  public boolean setIndex ( String theIndexName,
			    String theStringSubcomponentToIndex )
  {
    int i;

    for ( i=0;  i < getSubcomponentsCount();  i++ )
    {
      if (   ( isSubcomponentAString ( i ) )
	  && ( getStringSubcomponent ( i )
	         . equals ( theStringSubcomponentToIndex ) ) )
      {
	setIndex ( theIndexName, i );
	return true;
      }
    }

    System.err.println ( "[DataComponent:setIndex]  "
			 + "Unable to find subcomponent named \""
			 + theStringSubcomponentToIndex + "\"."   );
    return false;
  }


  public boolean removeIndex ( String theIndexName )
  {
    return getIndexData() . remove ( theIndexName ) != null;
  }

	/* Used to set up non-significant tokens that should be printed
	 * before the actual DataStatement.
	 * Implemented, principlely in DataStatement's generateLabels()
	 */
  public void setStartIndex ( )  { setIndex ( getFirstTokenIndex() ); }
  public void setStartIndex ( int theIndex )
			       { setIndex ( getFirstTokenIndex(), theIndex ); }
  public void setStartIndex ( Integer theIndex )
			       { setIndex ( getFirstTokenIndex(), theIndex ); }

  public String getFirstTokenIndex() { return DataComponent.FIRST_TOKEN_INDEX;}






	/* Convenience Methods for Subclasses
	 *   ( If a subclass permits the toString/isValid/generating
	 *     of subsets, these methods can/should be overriden... )
	 * Note: Invoked by default DataComponent methods.
	 */
  public String getWarnString ( int theObjectSubset )
  {
    return
        "theObjectSubset (" + theObjectSubset
      + ") != DataComponent.ENTIRE_OBJECT ("
      + DataComponent.ENTIRE_OBJECT
      + ") or DataComponent.CXX_CODE ("
      + DataComponent.CXX_CODE
      + ") or DataComponent.CXX_HEADER ("
      + DataComponent.CXX_HEADER
      + ") or DataComponent.INLINED_CXX_CODE ("
      + DataComponent.INLINED_CXX_CODE
      + ") or DataComponent.CXX_CODE_NO_FUNCTIONS ("
      + DataComponent.CXX_CODE_NO_FUNCTIONS
      + ") or DataComponent.CXX_HEADER_INLINED_FUNCTIONS ("
      + DataComponent.CXX_HEADER_INLINED_FUNCTIONS
      + ") or DataComponent.CXX_CODE_AND_HEADER ("
      + DataComponent.CXX_CODE_AND_HEADER
      + ") or DataComponent.CXX_BARE_HEADER ("
      + DataComponent.CXX_BARE_HEADER
      + ") or DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY ("
      + DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY
      + ") or DataComponent.CXX_HEADER_DISTRIBUTED_ONLY ("
      + DataComponent.CXX_HEADER_DISTRIBUTED_ONLY
      + ") or DataComponent.CXX_CODE_DISTRIBUTED_ONLY ("
      + DataComponent.CXX_CODE_DISTRIBUTED_ONLY
      + ").";
  }

  public boolean isCxxHeaderSubset ( int theObjectSubset )
  {
    return
      (   ( theObjectSubset == DataComponent.CXX_HEADER                   )
       || ( theObjectSubset == DataComponent.INLINED_CXX_CODE             )
       || ( theObjectSubset == DataComponent.CXX_HEADER_INLINED_FUNCTIONS )
       || ( theObjectSubset == DataComponent.CXX_BARE_HEADER              )
       || ( theObjectSubset
	             == DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY )
       || ( theObjectSubset == DataComponent.CXX_HEADER_DISTRIBUTED_ONLY  ) );
  }

  public boolean isCxxCodeSubset ( int theObjectSubset )
  {
    return
      (   ( theObjectSubset == DataComponent.CXX_CODE                     )
       || ( theObjectSubset == DataComponent.CXX_CODE_NO_FUNCTIONS        )
       || ( theObjectSubset == DataComponent.CXX_CODE_AND_HEADER          )
       || ( theObjectSubset == DataComponent.CXX_CODE_DISTRIBUTED_ONLY    ) );
  }

  public boolean isCxxDistributedOnlySubset ( int theObjectSubset )
  {
    return
     (   (theObjectSubset == DataComponent.CXX_HEADER_INLINED_DISTRIBUTED_ONLY)
      || (theObjectSubset == DataComponent.CXX_HEADER_DISTRIBUTED_ONLY    )
      || (theObjectSubset == DataComponent.CXX_CODE_DISTRIBUTED_ONLY      ) );
  }

  public boolean isCxxSubset ( int theObjectSubset )
  {
    return (   isCxxHeaderSubset ( theObjectSubset )
	    || isCxxCodeSubset   ( theObjectSubset ) );
  }

  public boolean isValidObjectSubset ( int theObjectSubset )
  {
    return (   ( theObjectSubset == DataComponent.ENTIRE_OBJECT )
	    || ( isCxxSubset ( theObjectSubset )                ) );
  }

  public void   warnIfInvalidObjectSubset ( int     theObjectSubset,
					    String  theMethodName )
  {
    if ( isValidObjectSubset ( theObjectSubset ) == false )
    {
      System.err.println ( "[" + getClass() . getName()
			   + ":" + theMethodName + "]  Warning:  "
			   + getWarnString ( theObjectSubset )
			   + ".\n" );
    }
  }

  public String toString ( )
  {
    return toString ( DataComponent.ENTIRE_OBJECT, false );
  }

  public String toString ( int theObjectSubsetToString )
  {
    return toString ( theObjectSubsetToString, false );
  }

  public String toString ( boolean theShouldStripLeadingWhitespace )
  {
    return toString ( DataComponent.ENTIRE_OBJECT,
		      theShouldStripLeadingWhitespace );
  }

  public String toString ( int     theObjectSubsetToString,
			   boolean theShouldStripLeadingWhitespace )
  {
    	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToString, "toString" );

	/* Generate a new string.  */
    DataDestinationStringBuffer  dataDest = new DataDestinationStringBuffer();
    dataDest . setStripLeadingWhitespace ( theShouldStripLeadingWhitespace );
    generate ( dataDest, theObjectSubsetToString );
    return dataDest . getString();
  }


  public boolean isValid ( )
			    { return isValid ( DataComponent.ENTIRE_OBJECT ); }

	/* Returns false if this object is not currently valid. */
  public boolean isValid ( int theObjectSubsetToValidate )
  {
    	/* Print a warning if necessary... */
    warnIfInvalidObjectSubset ( theObjectSubsetToValidate, "isValid" );

	/* Are all our subcomponents valid? */
    for ( int i=0;  i<getSubcomponentsCount();  i++ )
    {
      if (    isSubcomponentADataComponent ( i )
	  && ( getDataComponentSubcomponent ( i ) . isValid() == false ) )
      {
	return false;
      }
    }

    return true;
  }


	/* Generates (writes) this object's data. */
  public abstract void generate ( DataDestination  theOutputDestination,
				  int              theObjectSubsetToGenerate );


	/* Generic interface to permit trivial generating of sets of
	 * subcomponets. (Typically, these are generated sequentially,
	 * and consist of comments or other fluff.)
	 */
  protected void initializeGenerateSubcomponentIndex()
					    { generateSubcomponentsIndex = 0; }

  protected void initializeGenerateSubcomponentIndex ( String theStartIndex )
		   { generateSubcomponentsIndex = getIndex ( theStartIndex ); }

  protected void initializeGenerateSubcomponentIndex ( int theStartIndex )
		   { generateSubcomponentsIndex = theStartIndex; }

  protected boolean getHasSubcomponentsToGenerateBefore ( String theStopIndex )
	 { return ( generateSubcomponentsIndex < getIndex ( theStopIndex ) )
	       && ( generateSubcomponentsIndex < getSubcomponentsCount()   ); }

  protected void generateSubcomponents (
				     String          theStopIndex,
				     DataDestination theOutputDestination,
				     int             theObjectSubsetToGenerate,
				     boolean         theInsertSpaces )
  {
    generateSubcomponents ( generateSubcomponentsIndex,
			    getIndex ( theStopIndex ),
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    theInsertSpaces );

    generateSubcomponentsIndex = Math.max ( generateSubcomponentsIndex,
					    getIndex ( theStopIndex )  );
  }

  protected void generateSubcomponents (
				     int             theStopIndex,
				     DataDestination theOutputDestination,
				     int             theObjectSubsetToGenerate,
				     boolean         theInsertSpaces )
  {
    generateSubcomponents ( generateSubcomponentsIndex,
			    theStopIndex,
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    theInsertSpaces );

    generateSubcomponentsIndex = Math.max ( generateSubcomponentsIndex,
					    theStopIndex               );
  }

  protected void generateAllRemainingSubcomponents (
				     DataDestination theOutputDestination,
				     int             theObjectSubsetToGenerate,
				     boolean         theInsertSpaces )
  {
    generateSubcomponents ( generateSubcomponentsIndex,
			    getSubcomponentsCount(),
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    theInsertSpaces );

    generateSubcomponentsIndex = Math.max ( generateSubcomponentsIndex,
					    getSubcomponentsCount()    );
  }

  protected void generateSubcomponents (
				     String          theStartIndex,
				     String          theStopIndex,
				     DataDestination theOutputDestination,
				     int             theObjectSubsetToGenerate,
				     boolean         theInsertSpaces )
  {
    generateSubcomponents ( getIndex ( theStartIndex ),
			    getIndex ( theStopIndex  ),
			    theOutputDestination,
			    theObjectSubsetToGenerate,
			    theInsertSpaces );
  }
	/* Generic method to generate out a subset of our subcomponents */
  protected void generateSubcomponents (
				     int             theStartIndex,
				     int             theEndIndex,
				     DataDestination theOutputDestination,
				     int             theObjectSubsetToGenerate,
				     boolean         theInsertSpaces )
  {
    for ( int  index = theStartIndex;
	  (   ( index < theEndIndex             )
	   && ( index < getSubcomponentsCount() ) );
	 index ++ )
    {
      if ( isSubcomponentAString ( index ) )
	theOutputDestination . write ( getStringSubcomponent ( index ) );

      else if ( isSubcomponentADataComponent ( index ) )
	getDataComponentSubcomponent ( index )
	  . generate ( theOutputDestination, theObjectSubsetToGenerate );
      else
      {
	System.err.println ( "[DataComponent:generateSubcomponents]  "
		     + "Warning:  Unrecognized subcomponent \""
		     + getSubcomponent ( index ) . getClass() . getName()
		     + "\".  Skipping..." );
      }
    }

    if (    theInsertSpaces
	 && ( theStartIndex >= theEndIndex ) )
      theOutputDestination . write ( DataComponent.SPACE_STRING );
  }



  public int countLeadingSubcomponentNewlines()
  {
    return countSubcomponentNewlines ( 0,
				       getSubcomponentsCount(),
				       DataComponent.ENTIRE_OBJECT,
				       true );
  }

  public int countSubcomponentNewlines ( 
				String   theStopIndex,
				int      theObjectSubsetToGenerate,
				boolean  theStopAtFirstNonWhitespaceCharacter )
  {
    return countSubcomponentNewlines ( generateSubcomponentsIndex,
				       getIndex ( theStopIndex ),
				       theObjectSubsetToGenerate,
				       theStopAtFirstNonWhitespaceCharacter );
  }

  public int countSubcomponentNewlines (
				int      theStopIndex,
				int      theObjectSubsetToGenerate,
				boolean  theStopAtFirstNonWhitespaceCharacter )
  {
    return countSubcomponentNewlines ( generateSubcomponentsIndex,
				       theStopIndex,
				       theObjectSubsetToGenerate,
				       theStopAtFirstNonWhitespaceCharacter );
  }

  public int countSubcomponentNewlines (
				String   theStartIndex,
				String   theAlternateStartIndex,
				String   theStopIndex,
				int      theObjectSubsetToGenerate,
				boolean  theStopAtFirstNonWhitespaceCharacter )
  {
    return countSubcomponentNewlines (
			      Math.max ( getIndex ( theStartIndex ),
					 getIndex ( theAlternateStartIndex ) ),
			      getIndex ( theStopIndex  ),
			      theObjectSubsetToGenerate,
			      theStopAtFirstNonWhitespaceCharacter );
  }


  public int countSubcomponentNewlines ( 
				int      theStartIndex,
				int      theStopIndex,
				int      theObjectSubsetToGenerate,
				boolean  theStopAtFirstNonWhitespaceCharacter )
  {
    int    newlineCount = 0;
    int    index, j;
    String string;

    for ( index = theStartIndex
	    ;
	  (   ( index < theStopIndex            )
	   && ( index < getSubcomponentsCount() ) )
	    ;
	  index ++ )
    {
      if ( isSubcomponentAString ( index ) )
      {
	string = getStringSubcomponent ( index );
      }
      else if ( isSubcomponentADataComponent ( index ) )
      {
	DataDestinationStringBuffer  dataDest
	  = new DataDestinationStringBuffer();

	getDataComponentSubcomponent ( index )
	  . generate ( dataDest, theObjectSubsetToGenerate );

	string = dataDest.getString();
      }
      else
      {
	System.err.println ( "[DataComponent:countSubcomponentNewlines]  "
		     + "Warning:  Unrecognized subcomponent \""
		     + getSubcomponent ( index ) . getClass() . getName()
		     + "\".  Skipping..." );
	continue;
      }


      for ( j=0;   j < string.length();   j++ )
      {
	if ( string.charAt ( j ) == '\n' )
	  newlineCount++;

	else if (   ( theStopAtFirstNonWhitespaceCharacter         == true  )
		 && ( Character.isWhitespace( string.charAt( j ) ) == false ) )
	  break;  /* Exit FOR(j) loop */
      } /* FOR ( j = 0;  j < string.length();  j ++ ) */

	/* Did we find a non-whitespace character? */
      if ( j < string.length() )
	break;  /* Exit FOR(index) loop */

    } /* FOR ( index=theStartIndex; index < theStopIndex (& Max); index++ ) */

    return newlineCount;
  }

}

