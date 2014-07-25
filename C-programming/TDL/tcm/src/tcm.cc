/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcm.cc
 *
 * ABSTRACT: Public include file for the Task Control Management library.
 *           Defines the TCM API.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcm.cc,v $ 
 * $Revision: 1.36 $
 * $Date: 2009/05/04 19:44:49 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcm.cc,v $
 * Revision 1.36  2009/05/04 19:44:49  reids
 * Changed to using snprintf to avoid corrupting the stack on overflow
 *
 * Revision 1.35  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.34  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.33  2003/10/23 12:18:46  reids
 * Fixed several memory leaks, including one big one caused by a bug in
 *   g++ version 3.2.3 (this necessitated a change to some tcm.h signatures,
 *   hence the new TCM minor version number -- 2.8.0)
 *
 * Revision 1.32  2003/06/24 17:06:13  da0g
 * Commented out default value in TCM_SetAllowInfiniteTimeouts().
 *
 * Revision 1.31  2003/04/17 21:10:01  da0g
 * Changed clearQueues/TCM_ProcessAgenda to allow for
 *   return-when-all-work-is-done option.
 * Added silentlyAutoCorrectArguments option to TCM_ProcessAgenda to
 *   address waitingAllowed/relativeTimeout swappage.
 *
 * Revision 1.30  2003/01/29 20:30:23  da0g
 * Added userData to TaskTree.
 *
 * Revision 1.29  2002/12/23 02:25:25  da0g
 * Added Node-Class routines, User-specified Task-Thread mapping routines.
 *
 * Revision 1.28  2002/09/16 22:48:54  da0g
 * Added infinite-loop detection (and overrides).
 *
 * Revision 1.27  2002/07/11 03:52:23  da0g
 * Addressed String = (char*) vs (const char *) issues.
 * Addressed minor threading issues.
 * Added NULL_CHAR macro.
 * Altered monitorActivationName() (removed static variable),
 *   TCM_AllocateCompleteMonitorNode() now exploits monitorActivationName().
 *
 * Revision 1.26  2002/06/26 16:49:56  reids
 * Made a distinction between the type-name and instance-name of a node.
 *  Enable instance name of node to be set.
 *  Added casts to satisfy gcc 3.0.
 *
 * Revision 1.25  2002/05/10 03:27:59  reids
 * tcm.h
 *
 * Revision 1.24  2002/03/22 02:27:04  da0g
 * Added Exception-Handler-Ordering code.
 * Added TCM_IsDistributedNode().
 *
 * Revision 1.23  2001/10/23 22:52:58  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.22  2001/07/24 12:49:04  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.21  2001/04/04 14:26:13  reids
 * Task tree nodes are now garbage collected, by default, after they are
 *   completely achieved.  Can change this behavior using TCM_SetPersistence.
 * Also, cleaned up some memory leaks: Now seems to lose little, if any, memory
 *
 * Revision 1.20  2000/08/18 17:13:45  da0g
 * Bugfix: monitorDereference() not being used.
 *
 * Revision 1.19  2000/07/05 23:12:22  da0g
 * Added rudimentary Run-Time-Type-Identification to _Action, Monitor_Action,
 * and Polling_Monitor_Action.
 *
 * Modified monitor actions & tcm.h monitor interface to support the
 * allocate-then-set-attributes model of TDL.
 *
 * Polling_Monitor_Action with an infinite time period will behave as
 * standard non-polling Monitor_Action.
 *
 * Added Get/Set-Actual-Action functions.  They behave like the
 * standard functions, only they dereference monitors and get/set
 * the Monitor_Action's (or monitor instance's) action.
 *
 * Added a means of accessing Ref-Count Failure Data for
 * TDL exception support.
 *
 * Revision 1.18  2000/01/19 21:26:43  reids
 * Added two new top-level functions:
 *   TCM_IsDoneHandling(ref) -- returns TRUE if the ref has raised success or
 * 			     failure.
 *   TCM_IsPostponed(ref) -- returns TRUE if the ref's action has finished
 * 		          executing, but the node has not yet raised
 * 			  success or failure.
 *
 * Revision 1.17  1999/06/06 13:48:09  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.16  1999/03/02 01:31:13  da0g
 * Updated TCM_FailureData to use getNodeRefCountData
 *
// Revision 1.15  98/12/21  17:56:11  reids
// Added TCM_GetAction.
// 
 * Revision 1.14  1998/12/16 03:08:20  reids
 * Added support for "on termination" functions.
 *   Also enabled tca.h and tcm.h to co-exist (needed to change values
 *   of several constants).
 *
// Revision 1.13  98/12/14  15:20:08  da0g
// Added relativeTimeout to TCM_ProcessAgenda
// 
// Revision 1.12  98/10/30  11:16:32  da0g
// Added ExternalEventBypassHandler.
// 
 * Revision 1.11  1998/09/15 18:45:22  da0g
 * Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
 *
 * Revision 1.10  1998/07/14 17:21:02  reids
 * Changed TCM_DelayUntilAfter to TCM_DelayForAfter, changed TCM_
 *   TerminateAtAfter to TCM_TerminateInAfter to TCM_DelayForAfter, and
 *   changed order of arguments.
 * Added TCM_ActivateInAfter.
 *
// Revision 1.9  98/04/21  12:46:10  reids
// Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
//   particular event occurs.
// Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
//   a node by waiting some msecs after a particular event has occurred.
// 
// Revision 1.8  98/03/06  13:12:45  reids
// Added a way to stop the compiler from complaining about unused function
//   arguments (apparently, g++ ignores the "#pragma unused" statement).
// 
// Revision 1.7  98/01/30  14:50:59  reids
// Updated to compile under gcc 2.7.2 and under Linux.
// Also, made STRING "const char *" and changed API to take const arguments,
//   where applicable.
// 
// Revision 1.6  97/12/30  12:29:02  reids
// Added option to *not* wait for timeouts and external events.
//   Added flag to indicate whether node raised an exception (ie, failed).
//   Added a "timer" monitor, which is like a polling monitor, except when
//     activated it just invokes a function (rather than adding a new node)
// 
// Revision 1.5  97/12/29  17:06:32  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.4  97/12/22  16:53:03  reids
// Basically, added "data" field for CALLBACK_ACTION,
//  and started using "nodeData" for the activation data associated
//  with monitors, and the failure data associated with exceptions.
// 
// Revision 1.3  97/12/18  00:21:49  reids
// Changing ACTION_PTR to a handle, to aid in garbage collection.
// 
// Revision 1.2  97/12/04  17:50:21  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:39  reids
// First release of TCM -- seems to be a stable version
// 
 *
 *****************************************************************************/

#include "tcmGlobal.h"
#include "taskTree.h"
#include "tplConstr.h"
#include "exception.h"
#include "monitor.h"
#include "tcm.h"
#include "tcmThread.h"

const TCM_Task_Tree_Ref NullRef;

#define RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, fnName) \
  if (!checkNodeValidity(nodeRef, fnName)) {           \
    TCM_UnlockMasterMutex ( fnName );                  \
    return TCM_Error;                                  \
  }

#define RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, fnName, retVal) \
  if (!checkNodeValidity(nodeRef, fnName)) {                   \
    TCM_UnlockMasterMutex ( fnName );                          \
    return retVal;                                             \
  }

#define RETURN_UNLOCK_IF_INVALID_TIME(timePoint, fnName) \
  if (!checkTimePointValidity(timePoint, fnName)) {      \
    TCM_UnlockMasterMutex ( fnName );                    \
    return TCM_Error;                                    \
  }

#define RETURN_UNLOCK_IF_NULL(value, fnName)    \
  if (value == NULL) {                          \
    tcmWarning("%s: NULL argument\n", fnName);  \
    TCM_UnlockMasterMutex ( fnName );           \
    return TCM_Error;                           \
  }

#define RETURN_UNLOCK_IF_INVALID_MONITOR(theNodeRef,    theFunctionName,      \
			                 theCheckAction,theCheckPollingAction,\
				         theTaskTreeMonitorNodeReturnValue   )\
  if (checkMonitorValidity( theNodeRef,     theFunctionName,                  \
			    theCheckAction, theCheckPollingAction,            \
			    theTaskTreeMonitorNodeReturnValue     ) == FALSE )\
  {                                                                           \
    TCM_UnlockMasterMutex ( theFunctionName );                                \
    return TCM_Error;                                                         \
  }

/*****************************************************************
 * 
 * Utility functions.
 *
 *****************************************************************/

static BOOLEAN checkNodeValidity (const TCM_Task_Tree_Ref &nodeRef,
				  STRING functionName)
{
  TCM_LockMasterMutex ( "checkNodeValidity" );

  BOOLEAN valid = validRef(nodeRef);
  if (!valid)
    tcmWarning("%s: Reference to NULL node\n", functionName);

  TCM_UnlockMasterMutex ( "checkNodeValidity" );
  return valid;
}

static BOOLEAN checkTimePointValidity (const TCM_Point &timePoint,
				       STRING functionName)
{
  TCM_LockMasterMutex ( "checkTimePointValidity" );

  BOOLEAN valid = timePoint.valid();
  if (!valid)
    tcmWarning("%s: Reference to invalid time point\n", functionName);

  TCM_UnlockMasterMutex ( "checkTimePointValidity" );
  return valid;
}

static BOOLEAN checkMonitorValidity (
		 const TCM_Task_Tree_Ref &  theNodeRef,
		 STRING                     theFunctionName,
		 BOOLEAN                    theCheckAction,
		 BOOLEAN                    theCheckPollingAction,
		 Task_Tree_Monitor_Node * & theTaskTreeMonitorNodeReturnValue )
{
  TCM_LockMasterMutex ( "checkMonitorValidity" );

  if ( checkNodeValidity ( theNodeRef, theFunctionName ) == FALSE )
  {
    theTaskTreeMonitorNodeReturnValue = (Task_Tree_Monitor_Node *) NULL;
    TCM_UnlockMasterMutex ( "checkMonitorValidity" );
    return FALSE;
  }

  theTaskTreeMonitorNodeReturnValue = monitorDereference ( theNodeRef );

  if ( theTaskTreeMonitorNodeReturnValue == (Task_Tree_Monitor_Node *)NULL )
  {
    tcmWarning ( "%s: %s is not a monitor or monitor-activation.\n",
		 theFunctionName, theNodeRef->instanceName() );
    TCM_UnlockMasterMutex ( "checkMonitorValidity" );
    return FALSE;
  }

  if ( theCheckAction == TRUE )
  {
    if ( TCM_GetAction( theTaskTreeMonitorNodeReturnValue ) == (_Action *)NULL)
    {
      tcmWarning ( "%s: %s has no monitor-action\n",
		   theFunctionName,
		   theTaskTreeMonitorNodeReturnValue->instanceName() );
      TCM_UnlockMasterMutex ( "checkMonitorValidity" );
      return FALSE;
    }

    if ( theCheckPollingAction == FALSE )
    {
      if (   ( streq( TCM_GetAction( theTaskTreeMonitorNodeReturnValue)
		        -> TCM_getActionName(),
		      Polling_Monitor_Action::TCM_getStaticName()) == FALSE )
	  && ( streq( TCM_GetAction( theTaskTreeMonitorNodeReturnValue)
		        -> TCM_getActionName(),
		      Monitor_Action::TCM_getStaticName()        ) == FALSE ) )
      {
	tcmWarning ( "%s: %s has a %s action, which is neither a "
		     "Monitor_Action nor Polling_Monitor_Action.\n",
		     theFunctionName,
		     theTaskTreeMonitorNodeReturnValue -> instanceName(),
		     TCM_GetAction ( theTaskTreeMonitorNodeReturnValue )
		       -> TCM_getActionName() );
	TCM_UnlockMasterMutex ( "checkMonitorValidity" );
	return FALSE;
      }
    }
    else
    {
      if ( streq ( TCM_GetAction( theTaskTreeMonitorNodeReturnValue )
		     -> TCM_getActionName(),
		   Polling_Monitor_Action::TCM_getStaticName() ) == FALSE )
      {
	tcmWarning ( "%s: %s has a %s action, which is not a "
		     "Polling_Monitor_Action.\n",
		     theFunctionName,
		     theTaskTreeMonitorNodeReturnValue -> instanceName(),
		     TCM_GetAction ( theTaskTreeMonitorNodeReturnValue )
		       -> TCM_getActionName() );
	TCM_UnlockMasterMutex ( "checkMonitorValidity" );
	return FALSE;
      }
    } /* if ( theCheckPollingAction == FALSE ) ... ELSE ... */
  } /* if ( theCheckAction == TRUE ) */

  TCM_UnlockMasterMutex ( "checkMonitorValidity" );
  return TRUE;
}

/*****************************************************************
 * 
 * Functions having to do with creating task tree nodes
 *   and defining handlers/callbacks.
 *
 *****************************************************************/

const TCM_Task_Tree_Ref & TCM_RootNode (void)
{
  TCM_LockMasterMutex ( "TCM_RootNode" );

  const TCM_Task_Tree_Ref & returnValue
    = GET_TCM_GLOBAL(rootNode);

  TCM_UnlockMasterMutex ( "TCM_RootNode" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_AllocateGoalNode (STRING nodeTypeName)
 *
 * DESCRIPTION: Create a new goal node, with the given name
 *
 * NOTE: The node is not connected to the task tree, and is not runnable.
 *
 **************************************************************************/

TCM_Task_Tree_Ref TCM_AllocateGoalNode (STRING nodeTypeName)
{
  TCM_LockMasterMutex ( "TCM_AllocateGoalNode" );

  TCM_Task_Tree_Ref returnValue
    = new Task_Tree_Goal_Node(nodeTypeName);

  TCM_UnlockMasterMutex ( "TCM_AllocateGoalNode" );
  return returnValue;
}

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocateGoalNode (STRING nodeTypeName,
					STRING instanceName)
{
  TCM_LockMasterMutex ( "TCM_AllocateGoalNode" );

  TCM_Task_Tree_Ref nodeRef = TCM_AllocateGoalNode(nodeTypeName);
  TCM_SetInstanceName(nodeRef, instanceName);

  TCM_UnlockMasterMutex ( "TCM_AllocateGoalNode" );
  return nodeRef;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_AllocateCommandNode (STRING nodeTypeName)
 *
 * DESCRIPTION: Create a new command node, with the given name
 *
 * NOTE: The node is not connected to the task tree, and is not runnable.
 *
 **************************************************************************/

TCM_Task_Tree_Ref TCM_AllocateCommandNode (STRING nodeTypeName)
{
  TCM_LockMasterMutex ( "TCM_AllocateCommandNode" );

  TCM_Task_Tree_Ref returnValue
    = new Task_Tree_Command_Node(nodeTypeName);

  TCM_UnlockMasterMutex ( "TCM_AllocateCommandNode" );
  return returnValue;
}

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocateCommandNode (STRING nodeTypeName,
					   STRING instanceName)
{
  TCM_LockMasterMutex ( "TCM_AllocateCommandNode" );

  TCM_Task_Tree_Ref nodeRef = TCM_AllocateCommandNode(nodeTypeName);
  TCM_SetInstanceName(nodeRef, instanceName);

  TCM_UnlockMasterMutex ( "TCM_AllocateCommandNode" );
  return nodeRef;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type 
 *           TCM_DeallocateNode (const TCM_Task_Tree_Ref &nodeRef)
 *
 * DESCRIPTION: Destroy the node.
 *
 * NOTE: Only works if the node is not connected to the task tree,
 *       and has no children!  Otherwise, use "TCM_Terminate"
 *
 **************************************************************************/

TCM_Return_Type TCM_DeallocateNode (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_DeallocateNode" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_DeallocateNode");

  TCM_Return_Type returnValue
    = (nodeRef->deallocate() ? TCM_Ok : TCM_Error);

  TCM_UnlockMasterMutex ( "TCM_DeallocateNode" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_SetAction (const TCM_Task_Tree_Ref &nodeRef, 
 *                                          const TCM_Action_Ref action);
 *
 * DESCRIPTION: Set the action (handler function) for the given node.
 *
 **************************************************************************/

TCM_Return_Type TCM_SetAction (const TCM_Task_Tree_Ref &nodeRef, 
			       const TCM_Action_Ref action)
{
  TCM_LockMasterMutex ( "TCM_SetAction" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SetAction");

  nodeRef->setAction(action);

  TCM_UnlockMasterMutex ( "TCM_SetAction" );
  return TCM_Ok;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_SetActualAction (
 *                                          const TCM_Task_Tree_Ref &nodeRef, 
 *                                          const TCM_Action_Ref action);
 *
 * DESCRIPTION: Set the action (handler function) for the given node.
 *              Dereferences Monitors to set the Activation-Action.
 *              (Assumes monitor already has a {Polling_}Monitor_Action.
 *               Ie: that it been created with TCM_AllocateCompleteMonitorNode)
 *
 **************************************************************************/

TCM_Return_Type TCM_SetActualAction (const TCM_Task_Tree_Ref &nodeRef, 
				     const TCM_Action_Ref action)
{
  TCM_LockMasterMutex ( "TCM_SetActualAction" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SetActualAction");

  TCM_Return_Type returnValue;

  if ( nodeRef -> isMonitor() )
  {
    if ( TCM_GetAction ( nodeRef ) == (_Action *)NULL )
    {
      returnValue = TCM_Error;
    }
    else
    {
      ( (Monitor_Action*) (TCM_GetAction ( nodeRef ) . operator*()) )
	-> setActivationAction( action );
      returnValue = TCM_Ok;
    }
  }
  else
  {
    returnValue = TCM_SetAction ( nodeRef, action );
  }

  TCM_UnlockMasterMutex ( "TCM_SetActualAction" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Action_Ref TCM_GetAction (const TCM_Task_Tree_Ref &nodeRef );
 *
 * DESCRIPTION: Get the action (handler function) for the given node.
 *
 **************************************************************************/

TCM_Action_Ref TCM_GetAction (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_GetAction" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_GetAction", NULL);

  TCM_Action_Ref returnValue
    = nodeRef->getAction();

  TCM_UnlockMasterMutex ( "TCM_GetAction" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Action_Ref TCM_GetActualAction (
 *                                          const TCM_Task_Tree_Ref &nodeRef );
 *
 * DESCRIPTION: Get the actual action (handler function) for the given node.
 *              Dereferences Monitors to return the Activation-Action.
 *
 **************************************************************************/

TCM_Action_Ref TCM_GetActualAction (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_GetActualAction" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_GetActualAction", NULL);

  TCM_Action_Ref  returnValue;

  if ( nodeRef -> isMonitor() )
  {
    if ( TCM_GetAction ( nodeRef ) == (_Action *)NULL )
      returnValue = (_Action *)NULL;
    else
      returnValue
	= ( (Monitor_Action*) (TCM_GetAction ( nodeRef ) . operator*()) )
	     -> getActivationAction();
  }
  else
  {
    returnValue = TCM_GetAction ( nodeRef );
  }

  TCM_UnlockMasterMutex ( "TCM_GetActualAction" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_SetData (const TCM_Task_Tree_Ref &nodeRef, 
 *                                        const void *data);
 *
 * DESCRIPTION: Set the data associated with the given node.
 *
 * Notes:  TCM_SetData invokes Task_Tree_node::setNodeData().
 *         This method is also invoked by monitors and exceptions for
 *         other purposes.  Consider using TCM_SetUserData().
 *
 **************************************************************************/

TCM_Return_Type TCM_SetData (const TCM_Task_Tree_Ref &nodeRef,
			     const void *data)
{
  TCM_LockMasterMutex ( "TCM_SetData" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SetData");

  nodeRef->setNodeData(data);

  TCM_UnlockMasterMutex ( "TCM_SetData" );
  return TCM_Ok;
}

const void * TCM_GetUserData (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_GetUserData" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_GetUserData", (void*)NULL);

  const void * returnValue = nodeRef->getUserData();
  
  TCM_UnlockMasterMutex ( "TCM_GetUserData" );
  return returnValue;
}

TCM_Return_Type TCM_SetUserData (const TCM_Task_Tree_Ref &nodeRef,
				 const void *data)
{
  TCM_LockMasterMutex ( "TCM_SetUserData" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SetUserData");

  nodeRef->setUserData(data);

  TCM_UnlockMasterMutex ( "TCM_SetUserData" );
  return TCM_Ok;
}

const void * TCM_GetActualUserData (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_GetActualUserData" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_GetActualUserData", (void*)NULL);

  const void * returnValue;

    /* For monitors, we want the parent classes user-data */
  if ( TCM_NodeClassType ( nodeRef ) == TCM_Monitor )
    returnValue = TCM_Parent(nodeRef)->getUserData();
  else
    returnValue = nodeRef->getUserData();

  TCM_UnlockMasterMutex ( "TCM_GetActualUserData" );
  return returnValue;
}

TCM_Return_Type TCM_SetActualUserData (const TCM_Task_Tree_Ref &nodeRef,
				       const void *data)
{
  TCM_LockMasterMutex ( "TCM_SetActualUserData" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SetActualUserData");

    /* For monitors, we want the parent classes user-data */
  if ( TCM_NodeClassType ( nodeRef ) == TCM_Monitor )
    TCM_Parent(nodeRef)->setUserData(data);
  else
    nodeRef->setUserData(data);

  TCM_UnlockMasterMutex ( "TCM_SetActualUserData" );
  return TCM_Ok;
}

/**************************************************************************
 *
 * FUNCTION: TCM_InsertNode (const TCM_Task_Tree_Ref &parentRef,
 *			     const TCM_Task_Tree_Ref &childRef,
 *			     BOOLEAN isLastChild);
 *
 * DESCRIPTION: Add the childRef node as a child of the parentRef, and
 *              enable the childRef to be run.
 *
 **************************************************************************/

TCM_Return_Type TCM_InsertNode (const TCM_Task_Tree_Ref &parentRef,
				const TCM_Task_Tree_Ref &childRef,
				BOOLEAN isLastChild)
{
  TCM_LockMasterMutex ( "TCM_InsertNode" );
  RETURN_UNLOCK_IF_INVALID_NODE(parentRef, "TCM_InsertNode");
  RETURN_UNLOCK_IF_INVALID_NODE(childRef, "TCM_InsertNode");

  taskTreeInsert(*parentRef, *childRef, isLastChild);

  TCM_UnlockMasterMutex ( "TCM_InsertNode" );
  return TCM_Ok;
}

/****************************************************************
 *
 * The following four functions are shorthands for a sequence of
 *  allocate/setAction/insert calls.
 ****************************************************************/

TCM_Task_Tree_Ref TCM_CreateGoalNode (const TCM_Task_Tree_Ref &parentRef,
				      STRING nodeTypeName,
				      const TCM_Action_Ref &action,
				      BOOLEAN isLastChild)
{
  TCM_LockMasterMutex ( "TCM_CreateGoalNode" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_CreateGoalNode", NullRef);

  TCM_Task_Tree_Ref nodeRef(new Task_Tree_Goal_Node(nodeTypeName, action));
  taskTreeInsert(*parentRef, *nodeRef, isLastChild);

  TCM_UnlockMasterMutex ( "TCM_CreateGoalNode" );
  return nodeRef;
}

TCM_Task_Tree_Ref TCM_CreateGoalNode (const TCM_Task_Tree_Ref &parentRef,
				      STRING nodeTypeName, 
				      const void *callbackData,
				      CALLBACK_FN_TYPE callback,
				      BOOLEAN isLastChild)
{
  TCM_LockMasterMutex ( "TCM_CreateGoalNode" );

  TCM_Task_Tree_Ref returnValue
    = TCM_CreateGoalNode(parentRef, nodeTypeName,
			 new Callback_Action(callback, callbackData),
			 isLastChild);

  TCM_UnlockMasterMutex ( "TCM_CreateGoalNode" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_CreateCommandNode (const TCM_Task_Tree_Ref &parentRef,
					 STRING nodeTypeName,
					 const TCM_Action_Ref &action,
					 BOOLEAN isLastChild)
{
  TCM_LockMasterMutex ( "TCM_CreateCommandNode" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_CreateCommandNode", NullRef);

  TCM_Task_Tree_Ref nodeRef(new Task_Tree_Command_Node(nodeTypeName, action));
  taskTreeInsert(*parentRef, *nodeRef, isLastChild);

  TCM_UnlockMasterMutex ( "TCM_CreateCommandNode" );
  return nodeRef;
}

TCM_Task_Tree_Ref TCM_CreateCommandNode (const TCM_Task_Tree_Ref &parentRef,
					 STRING nodeTypeName, 
					 const void *callbackData,
					 CALLBACK_FN_TYPE callback,
					 BOOLEAN isLastChild)
{
  TCM_LockMasterMutex ( "TCM_CreateCommandNode" );

  TCM_Task_Tree_Ref returnValue
    = TCM_CreateCommandNode(parentRef, nodeTypeName,
			    new Callback_Action(callback, callbackData),
			    isLastChild);

  TCM_UnlockMasterMutex ( "TCM_CreateCommandNode" );
  return returnValue;
}

TCM_Return_Type TCM_Success (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_Success" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_Success");

  nodeRef->doneHandling();

#ifdef THREADED
  GET_TCM_GLOBAL(agenda).clearEventQueuesIfClearQueuesNotRunning();
#endif /* THREADED */

  TCM_UnlockMasterMutex ( "TCM_Success" );
  return TCM_Ok;
}

// If "true", the node persists even after it has been achieved.
// If "false" (now the default), it gets destroyed after achievement.
TCM_Return_Type TCM_SetPersistence (const TCM_Task_Tree_Ref & nodeRef,
				    BOOLEAN value)
{
  TCM_LockMasterMutex ( "TCM_SetPersistence" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SetPersistence");

  nodeRef->setPersistence(value);

  TCM_UnlockMasterMutex ( "TCM_SetPersistence" );
  return TCM_Ok;
}

/**************************************************************************
 *
 * Functions to map between a task tree node and its id number
 *
 **************************************************************************/
 
/**************************************************************************
 *
 * FUNCTION: int TCM_Remember (const TCM_Task_Tree_Ref &nodeRef)
 *
 * DESCRIPTION: Memoize the node, so that it can be recalled
 *
 **************************************************************************/
int TCM_Remember (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_Remember" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_Remember", -1);

  int returnValue
    = nodeRef->remember();

  TCM_UnlockMasterMutex ( "TCM_Remember" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_Recall (int refID)
 *
 * DESCRIPTION: Find the task tree node associated with the give id number
 *
 **************************************************************************/
TCM_Task_Tree_Ref TCM_Recall (int refID)
{
  TCM_LockMasterMutex ( "TCM_Recall" );

  TCM_Task_Tree_Ref returnValue;

  if (refID < 0) {
    returnValue = NullRef;
  } else {
    returnValue = GET_TCM_GLOBAL(rootNode)->recall(refID);
  }

  TCM_UnlockMasterMutex ( "TCM_Recall" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: void TCM_Forget (int refID)
 *
 * DESCRIPTION: Un-memoize the node, freeing up storage
 *
 **************************************************************************/
void TCM_Forget (int refID)
{
  TCM_LockMasterMutex ( "TCM_Forget" );

  if (refID >= 0) {
    GET_TCM_GLOBAL(rootNode)->forget(refID);
  }

  TCM_UnlockMasterMutex ( "TCM_Forget" );
}

/**************************************************************************
 *
 * FUNCTION: void TCM_Forget (int refID)
 *
 * DESCRIPTION: Combines TCM_Recall & TCM_Forget
 *
 **************************************************************************/

TCM_Task_Tree_Ref TCM_Retrieve (int refID)
{
  TCM_LockMasterMutex ( "TCM_Retrieve" );

  TCM_Task_Tree_Ref returnValue;

  if (refID < 0) {
    returnValue = NullRef;
  } else {
    returnValue = GET_TCM_GLOBAL(rootNode)->retrieve(refID);
  }

  TCM_UnlockMasterMutex ( "TCM_Retrieve" );
  return returnValue;
}

/*****************************************************************
 * 
 * Functions having to do with temporal constraints
 *
 *****************************************************************/

BOOLEAN TCM_Point::valid (void) const
{
  TCM_LockMasterMutex ( "TCM_Point::valid" );

  BOOLEAN returnValue
    = validRef(_interval._ref);

  TCM_UnlockMasterMutex ( "TCM_Point::valid" );
  return returnValue;
}

TCM_Interval TCM_HandlingOf (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_HandlingOf" );

  TCM_Interval interval(Handling_Interval, nodeRef);

  TCM_UnlockMasterMutex ( "TCM_HandlingOf" );
  return interval;
}

TCM_Interval TCM_PlanningOf (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_PlanningOf" );

  TCM_Interval interval(Planning_Interval, nodeRef);

  TCM_UnlockMasterMutex ( "TCM_PlanningOf" );
  return interval;
}

TCM_Interval TCM_AchievingOf (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_AchievingOf" );

  TCM_Interval interval(Achieving_Interval, nodeRef);

  TCM_UnlockMasterMutex ( "TCM_AchievingOf" );
  return interval;
}

TCM_Point TCM_StartOf (const TCM_Interval &interval)
{
  TCM_LockMasterMutex ( "TCM_StartOf" );

  TCM_Point point(Start_Point, interval);

  TCM_UnlockMasterMutex ( "TCM_StartOf" );
  return point;
}

TCM_Point TCM_EndOf (const TCM_Interval &interval)
{
  TCM_LockMasterMutex ( "TCM_EndOf" );

  TCM_Point point(End_Point, interval);

  TCM_UnlockMasterMutex ( "TCM_EndOf" );
  return point;
}

TCM_Return_Type TCM_AddConstraint (const TCM_Task_Tree_Ref &nodeRef,
				   int tplConstraints)
{
  TCM_LockMasterMutex ( "TCM_AddConstraint" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_AddConstraint");

  TCM_Return_Type returnValue;

  if (checkInconsistent(tplConstraints)) {
    tcmError("TCM_AddConstraint: Inconsistent temporal constraints %d\n",
	     tplConstraints);
    returnValue = TCM_Error;
  } else {
    nodeRef->addConstraints(tplConstraints);
    returnValue = TCM_Ok;
  }

  TCM_UnlockMasterMutex ( "TCM_AddConstraint" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_Serialize (const TCM_Task_Tree_Ref &beforeRef,
 *			                    const TCM_Task_Tree_Ref &afterRef)
 *
 * DESCRIPTION: Constrain the end of planning of "beforeRef" to be before 
 *              the start of planning of "afterRef", and the same for their
 *              achievement intervals.
 *
 **************************************************************************/
TCM_Return_Type TCM_Serialize (const TCM_Task_Tree_Ref &beforeRef,
			       const TCM_Task_Tree_Ref &afterRef)
{
  TCM_LockMasterMutex ( "TCM_Serialize" );
  RETURN_UNLOCK_IF_INVALID_NODE(beforeRef, "TCM_Serialize");
  RETURN_UNLOCK_IF_INVALID_NODE(afterRef, "TCM_Serialize");

  Time_Point end_ach_before(End_Point, Achieving_Interval, beforeRef);
  Time_Point start_plan_after(Start_Point, Planning_Interval, afterRef);
  Time_Point start_ach_after(Start_Point, Achieving_Interval, afterRef);

  constrainBefore(end_ach_before, start_plan_after);
  constrainBefore(end_ach_before, start_ach_after);

  TCM_UnlockMasterMutex ( "TCM_Serialize" );
  return TCM_Ok;
}

TCM_Return_Type TCM_DelayUntil (TCM_Point const &timePoint,
				TCM_Point const &delayTimePoint)
{
  TCM_LockMasterMutex ( "TCM_DelayUntil" );
  RETURN_UNLOCK_IF_INVALID_TIME(timePoint, "TCM_DelayUntil");
  RETURN_UNLOCK_IF_INVALID_TIME(delayTimePoint, "TCM_DelayUntil");

  Time_Point t1(timePoint), t2(delayTimePoint);
  constrainBefore(t2, t1);

  TCM_UnlockMasterMutex ( "TCM_DelayUntil" );
  return TCM_Ok;
}

TCM_Return_Type TCM_DelayUntil (TCM_Point const &timePoint,
				MSecs absoluteTime)
{
  TCM_LockMasterMutex ( "TCM_DelayUntil" );
  RETURN_UNLOCK_IF_INVALID_TIME(timePoint, "TCM_DelayUntil");

  Time_Point point(timePoint);
  constrainWhen(point, absoluteTime);

  TCM_UnlockMasterMutex ( "TCM_DelayUntil" );
  return TCM_Ok;
}

TCM_Return_Type TCM_DelayFor (TCM_Point const &timePoint, MSecs relativeWait)
{
  TCM_LockMasterMutex ( "TCM_DelayFor" );
  RETURN_UNLOCK_IF_INVALID_TIME(timePoint, "TCM_DelayFor");

  TCM_Return_Type returnValue
    = TCM_DelayUntil(timePoint, absoluteTime(relativeWait));

  TCM_UnlockMasterMutex ( "TCM_DelayFor" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_DelayForAfter (TCM_Point const &timePoint,
 *				                MSecs relativeWait,
 *                                              TCM_Point const &delayTimePoint)
 *
 * DESCRIPTION: Delay this timepoint for "relativeWait" msecs after 
 *              the "delayTimePoint" has passed.
 *
 **************************************************************************/

TCM_Return_Type TCM_DelayForAfter (TCM_Point const &timePoint,
				   MSecs relativeWait,
				   TCM_Point const &delayTimePoint)
{
  TCM_LockMasterMutex ( "TCM_DelayForAfter" );
  RETURN_UNLOCK_IF_INVALID_TIME(timePoint, "TCM_DelayForAfter");
  RETURN_UNLOCK_IF_INVALID_TIME(delayTimePoint, "TCM_DelayForAfter");

  Time_Point beforePoint(timePoint), afterPoint(delayTimePoint);

  constrainWhenAfter(beforePoint.nodeOf(), beforePoint.signalOf(),
		     afterPoint, relativeWait);

  TCM_UnlockMasterMutex ( "TCM_DelayForAfter" );
  return TCM_Ok;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_DelayUntilSignal (TCM_Point const &timePoint)
 *
 * DESCRIPTION: Don't enable this timepoint until TCM_Signal is invoked
 *              for that timepoint.
 *
 **************************************************************************/
TCM_Return_Type TCM_DelayUntilSignal (TCM_Point const &timePoint)
{
  TCM_LockMasterMutex ( "TCM_DelayUntilSignal" );
  RETURN_UNLOCK_IF_INVALID_TIME(timePoint, "TCM_DelayUntilSignal");
  
  Time_Point time(timePoint);
  Task_Tree_Ref node = time.nodeOf();
  
  node->addExpectedEvent(time.signalOf(), node);

  TCM_UnlockMasterMutex ( "TCM_DelayUntilSignal" );
  return TCM_Ok;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_Signal (TCM_Point const &timePoint)
 *
 * DESCRIPTION: Send an explicit signal to enable this timepoint.
 *
 **************************************************************************/
TCM_Return_Type TCM_Signal (TCM_Point const &timePoint)
{
  TCM_LockMasterMutex ( "TCM_Signal" );
  RETURN_UNLOCK_IF_INVALID_TIME(timePoint, "TCM_Signal");
  
  Time_Point time(timePoint);
  Task_Tree_Ref node = time.nodeOf();
  Event event(time.signalOf(), node);
  
  node->signal(event);

  TCM_UnlockMasterMutex ( "TCM_Signal" );
  return TCM_Ok;
}



TCM_Return_Type TCM_WaitUntil (TCM_Point const &waitTimePoint)
{
  TCM_LockMasterMutex ( "TCM_WaitUntil" );
  RETURN_UNLOCK_IF_INVALID_TIME(waitTimePoint, "TCM_WaitUntil");

  if (!TCM_IsPast(waitTimePoint))
  {
    Time_Point timepoint(waitTimePoint);

#ifdef THREADED
    if (
#endif /* THREADED */

    GET_TCM_GLOBAL(agenda).clearQueues(&timepoint, INFINITE_TIME, TRUE, FALSE)

#ifdef THREADED
	 == FALSE )
    {
      TCM_BlockThreadUntilPoint ( waitTimePoint );
    }
#else /*THREADED*/
    ;
#endif /* THREADED */

  } /* if (!TCM_IsPast(waitTimePoint)) */

  TCM_UnlockMasterMutex ( "TCM_WaitUntil" );
  return TCM_Ok;
}



TCM_Return_Type TCM_WaitUntil (MSecs absoluteTime)
{
  TCM_LockMasterMutex ( "TCM_WaitUntil" );

  if (!TCM_IsPast(absoluteTime))
  {

#ifdef THREADED
    if (
#endif /* THREADED */

    GET_TCM_GLOBAL(agenda).clearQueues(NULL, absoluteTime, TRUE, FALSE)

#ifdef THREADED
	 == FALSE )
    {
      MSecs currentTime = timeInMsecs();

      if ( absoluteTime == INFINITE_TIME )
      {
	tcmMessage ( "TCM_WaitUntil:  Unable to wait for infinite time.  "
		     "Another thread is running clearQueues [ProcessAgenda].");
      }
      else if  ( absoluteTime > currentTime )
      {
	MSecs  timeToWait = absoluteTime - currentTime;

	    /* MSecs could be a 32-bit or a 64-bit int.       *
	     * But sleep/usleep only takes 32-bit ints.       *
	     * Caveat:  4,000,000 seconds is about 46.3 days. *
	     * So this may never get tested too thoroughly... */
	for ( ;   timeToWait > 4000000000UL;   timeToWait -= 4000000000UL )
	  sleep ( 4000000UL );

	if ( timeToWait > 1000 )
	  sleep ( timeToWait / 1000 );

	if ( timeToWait % 1000 > 0 )
	  usleep ( timeToWait % 1000 );
      }
    }
#else /*THREADED*/
    ;
#endif /* THREADED */

  } /* if (!TCM_IsPast(absoluteTime)) */

  TCM_UnlockMasterMutex ( "TCM_WaitUntil" );
  return TCM_Ok;
}



TCM_Return_Type TCM_WaitFor (MSecs relativeWait)
{
  TCM_LockMasterMutex ( "TCM_WaitFor" );

  TCM_Return_Type returnValue
    = TCM_WaitUntil(absoluteTime(relativeWait));

  TCM_UnlockMasterMutex ( "TCM_WaitFor" );
  return returnValue;
}

BOOLEAN TCM_IsPast (TCM_Point const &timePoint)
{
  TCM_LockMasterMutex ( "TCM_IsPast" );

  Time_Point tp(timePoint);

  BOOLEAN returnValue
    = tp.isPast();

  TCM_UnlockMasterMutex ( "TCM_IsPast" );
  return returnValue;
}

BOOLEAN TCM_IsPast (MSecs absoluteTime)
{
  TCM_LockMasterMutex ( "TCM_IsPast" );

  BOOLEAN returnValue
    = (absoluteTime <= timeInMsecs());

  TCM_UnlockMasterMutex ( "TCM_IsPast" );
  return returnValue;
}

// The way this works is that the command node does not really finish
// until the timer sends it a "Done_Handling_Signal"
static void delayHnd (const TCM_Task_Tree_Ref &node, const void *data)
{
  TCM_LockMasterMutex ( "delayHnd" );
  tcmMessage("  Invoking DelayHnd\n");

  MSecs *delay = (MSecs *)data;
  addTimedEvent(*delay, node, Done_Handling_Signal);
  delete delay;

  TCM_Success(node);
  TCM_UnlockMasterMutex ( "delayHnd" );
}

// Assumes that "Time" and "ptr" are same length.
// Safer would be to create (and destroy) the data, and pass a true pointer.
// da0g, 9/2001:  Time is 64bit, ptr is 32bit.  This needs to be fixed.
TCM_Task_Tree_Ref TCM_CreateDelayCommand (const TCM_Task_Tree_Ref &parentRef,
					  MSecs relativeWait)
{
  TCM_LockMasterMutex ( "TCM_CreateDelayCommand" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_CreateDelayCommand", NullRef);

  MSecs *time = new MSecs;
  *time = absoluteTime(relativeWait);

  TCM_Task_Tree_Ref returnValue
    = TCM_CreateCommandNode(parentRef, "tcmDelay",
			    new Callback_Action(delayHnd, (void *)time));

  TCM_UnlockMasterMutex ( "TCM_CreateDelayCommand" );
  return returnValue;
}

TCM_Return_Type TCM_ClearTimeouts (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_ClearTimeouts" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_ClearTimeouts");

  nodeRef->clearTimeouts();

  TCM_UnlockMasterMutex ( "TCM_ClearTimeouts" );
  return TCM_Ok;
}


/*****************************************************************
 * 
 * Functions having to do with traversing task trees.
 *
 *****************************************************************/

TCM_Task_Tree_Ref TCM_Parent (const TCM_Task_Tree_Ref &childRef)
{
  TCM_LockMasterMutex ( "TCM_Parent" );
  RETURN_UNLOCK_ON_INVALID_NODE(childRef, "TCM_Parent", NullRef);

  TCM_Task_Tree_Ref returnValue
    = childRef->getParent();

  TCM_UnlockMasterMutex ( "TCM_Parent" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_FirstChild (const TCM_Task_Tree_Ref &parentRef)
{
  TCM_LockMasterMutex ( "TCM_FirstChild" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_FirstChild", NullRef);

  TCM_Task_Tree_Ref returnValue
    = parentRef->firstChild();

  TCM_UnlockMasterMutex ( "TCM_FirstChild" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_LastChild (const TCM_Task_Tree_Ref &parentRef)
{
  TCM_LockMasterMutex ( "TCM_LastChild" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_LastChild", NullRef);

  TCM_Task_Tree_Ref returnValue
    = parentRef->lastChild(FALSE);

  TCM_UnlockMasterMutex ( "TCM_LastChild" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_NextChild (const TCM_Task_Tree_Ref &childRef)
{
  TCM_LockMasterMutex ( "TCM_NextChild" );
  RETURN_UNLOCK_ON_INVALID_NODE(childRef, "TCM_NextChild", NullRef);

  TCM_Task_Tree_Ref returnValue
    = childRef->getParent()->nextChild(childRef);

  TCM_UnlockMasterMutex ( "TCM_NextChild" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_PreviousChild (const TCM_Task_Tree_Ref &childRef)
{
  TCM_LockMasterMutex ( "TCM_PreviousChild" );
  RETURN_UNLOCK_ON_INVALID_NODE(childRef, "TCM_PreviousChild", NullRef);

  TCM_Task_Tree_Ref returnValue
    = childRef->getParent()->previousChild(childRef);

  TCM_UnlockMasterMutex ( "TCM_PreviousChild" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_ChildNamed (const TCM_Task_Tree_Ref &parentRef,
				  STRING childName)
{
  TCM_LockMasterMutex ( "TCM_ChildNamed" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_ChildNamed", NullRef);
  
  Task_Tree_Node_Ptr child = parentRef->childNamed(childName);
  TCM_Task_Tree_Ref  returnValue
    = (child ? makeHandle(child) : NullRef);

  TCM_UnlockMasterMutex ( "TCM_ChildNamed" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_AncestorNamed (const TCM_Task_Tree_Ref &descendantRef,
				     STRING ancestorName)
{
  TCM_LockMasterMutex ( "TCM_AncestorNamed" );
  RETURN_UNLOCK_ON_INVALID_NODE(descendantRef, "TCM_AncestorNamed", NullRef);
  
  Task_Tree_Node_Ptr ancestor = descendantRef->ancestorNamed(ancestorName);
  TCM_Task_Tree_Ref  returnValue
    = (ancestor ? makeHandle(ancestor) : NullRef);

  TCM_UnlockMasterMutex ( "TCM_AncestorNamed" );
  return returnValue;
}

STRING TCM_NodeName (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_NodeName" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_NodeName", NULL);

  STRING returnValue = nodeRef->instanceName();

  TCM_UnlockMasterMutex ( "TCM_NodeName" );
  return returnValue;
}

STRING TCM_NodeTypeName (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_NodeTypeName" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_NodeName", NULL);

  STRING returnValue = nodeRef->nodeTypeName();

  TCM_UnlockMasterMutex ( "TCM_NodeTypeName" );
  return returnValue;
}

TCM_NodeClassType_Enum TCM_NodeClassType (const TCM_Task_Tree_Ref & nodeRef)
{
  TCM_LockMasterMutex ( "TCM_NodeClassType" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_NodeClassType", TCM_Unknown);

  TCM_NodeClassType_Enum  returnValue = TCM_Unknown;
  
  if      ( streq("Goal",    nodeRef->className()) )
    returnValue = TCM_Goal;

  else if ( streq("Command", nodeRef->className()) )
    returnValue = TCM_Command;

  else if ( streq("Virtual", nodeRef->className()) )
    returnValue = TCM_Virtual;

    /* Note: The Monitor's parent node is of type monitor... */
  if (   ( (returnValue == TCM_Goal) || (returnValue == TCM_Command) )
      && ( nodeRef != TCM_RootNode()                                 )
      && ( validRef ( TCM_Parent(nodeRef) )                          )
      && ( streq ( "Monitor", TCM_Parent(nodeRef)->className() )      )
      // Is not an on-terminate or exception node
      && ( !nodeRef->isTerminationDelayed()                          ) )
    returnValue = TCM_Monitor;

  TCM_UnlockMasterMutex ( "TCM_NodeClassType" );
  return returnValue;
}

STRING TCM_NodeClassTypeToString ( TCM_NodeClassType_Enum  theValue )
{
  switch ( theValue )
  {
    case TCM_Unknown: return STRING("TCM_Unknown");
    case TCM_Goal:    return STRING("TCM_Goal");
    case TCM_Command: return STRING("TCM_Command");
    case TCM_Monitor: return STRING("TCM_Monitor");
    case TCM_Virtual: return STRING("TCM_Virtual");
    default:          return STRING("INVALID_VALUE");
  }
}

STRING TCM_NodeClassString (const TCM_Task_Tree_Ref & nodeRef)
{
  return TCM_NodeClassTypeToString ( TCM_NodeClassType ( nodeRef ) );
}

/*****************************************************************
 * 
 * Set the "instance" name of the node to be different from its
 *  "type" name.  Works for distributed nodes, as well.
 *
 *****************************************************************/
TCM_Return_Type TCM_SetInstanceName (const TCM_Task_Tree_Ref &nodeRef,
				     const STRING newInstanceName)
{
  TCM_LockMasterMutex ( "TCM_SetInstanceName" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SetInstanceName");
  RETURN_UNLOCK_IF_NULL(newInstanceName, "TCM_SetInstanceName");

  nodeRef->setInstanceName(newInstanceName);

  TCM_UnlockMasterMutex ( "TCM_SetInstanceName" );
  return TCM_Ok;
}

/*****************************************************************
 * 
 * Functions having to do with terminating and suspending nodes
 *
 *****************************************************************/

TCM_Return_Type TCM_TerminateNode (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_TerminateNode" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_TerminateNode");

  TCM_Return_Type returnValue
    = (nodeRef->terminate() ? TCM_Ok : TCM_Error);

  TCM_UnlockMasterMutex ( "TCM_TerminateNode" );
  return returnValue;
}

TCM_Return_Type TCM_TerminateChildren (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_TerminateChildren" );
  TCM_Task_Tree_Ref childRef, nextChild;

  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_TerminateChildren");
  
  childRef = nodeRef->firstChild();
  while (childRef != NULL) {
    nextChild = nodeRef->nextChild(childRef);
    if (!childRef->terminate())
    {
      TCM_UnlockMasterMutex ( "TCM_TerminateChildren" );
      return TCM_Error;
    }
    childRef = nextChild;
  }

  TCM_UnlockMasterMutex ( "TCM_TerminateChildren" );
  return TCM_Ok;
}

TCM_Return_Type TCM_TerminateSiblings (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_TerminateSiblings" );

  TCM_Task_Tree_Ref parentRef, childRef, nextChild;

  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_TerminateSiblings");
  
  parentRef = nodeRef->getParent();

  if (parentRef == NULL)
  {
    TCM_UnlockMasterMutex ( "TCM_TerminateSiblings" );
    return TCM_Error;
  }

  childRef = parentRef->firstChild();

  while (childRef != NULL) {
    nextChild = parentRef->nextChild(childRef);
    if (childRef != nodeRef) {
      if (!childRef->terminate())
      {
	TCM_UnlockMasterMutex ( "TCM_TerminateSiblings" );
	return TCM_Error;
      }
    }
    childRef = nextChild;
  }

  TCM_UnlockMasterMutex ( "TCM_TerminateSiblings" );
  return TCM_Ok;
}

TCM_Return_Type TCM_SuspendNode (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_SuspendNode" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_SuspendNode");

  nodeRef->suspend();

  TCM_UnlockMasterMutex ( "TCM_SuspendNode" );
  return TCM_Ok;
}

TCM_Return_Type TCM_UnsuspendNode (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_UnsuspendNode" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_UnsuspendNode");

  nodeRef->unsuspend();

  TCM_UnlockMasterMutex ( "TCM_UnsuspendNode" );
  return TCM_Ok;
}

BOOLEAN TCM_IsSuspended (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_IsSuspended" );

  BOOLEAN returnValue
    = (   checkNodeValidity(nodeRef, "TCM_IsSuspended")
       && nodeRef->isSuspended());

  TCM_UnlockMasterMutex ( "TCM_IsSuspended" );
  return returnValue;
}

BOOLEAN TCM_IsDoneHandling (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_IsDoneHandling" );

  BOOLEAN returnValue
    = (   checkNodeValidity(nodeRef, "TCM_IsDoneHandling")
       && nodeRef->isDoneHandling());

  TCM_UnlockMasterMutex ( "TCM_IsDoneHandling" );
  return returnValue;
}

BOOLEAN TCM_IsPostponed (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_IsPostponed" );

  BOOLEAN returnValue
    = (   checkNodeValidity(nodeRef, "TCM_IsPostponed")
       && nodeRef->isPostponed());

  TCM_UnlockMasterMutex ( "TCM_IsPostponed" );
  return returnValue;
}

TCM_Return_Type TCM_TerminateAt (const TCM_Task_Tree_Ref &nodeRef,
				 TCM_Point const &terminateTimePoint)
{
  TCM_LockMasterMutex ( "TCM_TerminateAt" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_TerminateAt");
  RETURN_UNLOCK_IF_INVALID_TIME(terminateTimePoint, "TCM_TerminateAt");

  Time_Point point(terminateTimePoint);
  terminateAfter(nodeRef, point);

  TCM_UnlockMasterMutex ( "TCM_TerminateAt" );
  return TCM_Ok;
}

TCM_Return_Type TCM_TerminateAt (const TCM_Task_Tree_Ref &nodeRef,
				 MSecs absoluteTime)
{
  TCM_LockMasterMutex ( "TCM_TerminateAt" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_TerminateAt");

  terminateWhen(nodeRef, absoluteTime);

  TCM_UnlockMasterMutex ( "TCM_TerminateAt" );
  return TCM_Ok;
}

TCM_Return_Type TCM_TerminateIn (const TCM_Task_Tree_Ref &nodeRef,
				 MSecs relativeWait)
{
  TCM_LockMasterMutex ( "TCM_TerminateIn" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_TerminateIn");

  TCM_Return_Type returnValue
    = TCM_TerminateAt(nodeRef, absoluteTime(relativeWait));

  TCM_UnlockMasterMutex ( "TCM_TerminateIn" );
  return returnValue;
}

// Terminate this node in "relativeWait" msecs after 
//  the "terminateTimePoint" have passed.
TCM_Return_Type TCM_TerminateInAfter (const TCM_Task_Tree_Ref &nodeRef,
				      MSecs relativeWait,
				      TCM_Point const &terminateTimePoint)
{
  TCM_LockMasterMutex ( "TCM_TerminateInAfter" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_TerminateInAfter");
  RETURN_UNLOCK_IF_INVALID_TIME(terminateTimePoint, "TCM_TerminateInAfter");

  Time_Point point(terminateTimePoint);

  constrainWhenAfter(nodeRef, Start_Terminating_Signal, point, relativeWait);

  TCM_UnlockMasterMutex ( "TCM_TerminateInAfter" );
  return TCM_Ok;
}

TCM_Return_Type TCM_OnTermination (const TCM_Task_Tree_Ref &nodeRef,
				   const TCM_Task_Tree_Ref &terminationRef)
{
  TCM_LockMasterMutex ( "TCM_OnTermination" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_OnTermination");
  RETURN_UNLOCK_IF_INVALID_NODE(terminationRef, "TCM_OnTermination");

  nodeRef->addTerminationAction(terminationRef);

  TCM_UnlockMasterMutex ( "TCM_OnTermination" );
  return TCM_Ok;
}

TCM_Return_Type TCM_OnTermination (const TCM_Task_Tree_Ref &nodeRef,
				   STRING nodeTypeName,
				   const TCM_Action_Ref &terminationAction)
{
  TCM_LockMasterMutex ( "TCM_OnTermination" );

  TCM_Return_Type returnValue
    = TCM_OnTermination(nodeRef, new Task_Tree_Goal_Node(nodeTypeName, 
							 terminationAction));

  TCM_UnlockMasterMutex ( "TCM_OnTermination" );
  return returnValue;
}

TCM_Return_Type TCM_OnTermination (const TCM_Task_Tree_Ref &nodeRef,
				   STRING nodeTypeName,
				   const void *callbackData,
				   CALLBACK_FN_TYPE callback)
{
  TCM_LockMasterMutex ( "TCM_OnTermination" );

  TCM_Return_Type returnValue
    = TCM_OnTermination(nodeRef, nodeTypeName, 
			new Callback_Action(callback, callbackData));

  TCM_UnlockMasterMutex ( "TCM_OnTermination" );
  return returnValue;
}

/*****************************************************************
 * 
 * Functions having to do with exceptions
 *
 *****************************************************************/

TCM_Exception::~TCM_Exception ()
{
#ifdef TRACE_EXCEPTIONS
  tcmMessage("Deleting exception %s\n", name);
#endif
  free((void *)name);
}

TCM_Return_Type TCM_Failure (const TCM_Task_Tree_Ref &nodeRef, 
			     TCM_Exception *exception)
{
  TCM_LockMasterMutex ( "TCM_Failure" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_Failure");

  TCM_Return_Type returnValue;

  if (!exception) {
    tcmWarning("TCM_Failure: No exception specified\n");
    returnValue = TCM_Error;
  } else {
    raiseException(exception, nodeRef);
    returnValue = TCM_Ok;
  }
  TCM_UnlockMasterMutex ( "TCM_Failure" );
  return returnValue;
}

TCM_Return_Type TCM_Failure (const TCM_Task_Tree_Ref &nodeRef, 
			     STRING failure, void *failureData)
{
  TCM_LockMasterMutex ( "TCM_Failure" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_Failure");

  TCM_Return_Type returnValue;

  if (!failure || *failure == NULL_CHAR) {
    tcmWarning("TCM_Failure: No failure specified\n");
    returnValue = TCM_Error;
  } else {
    raiseException(create_TCM_Exception(failure, failureData), nodeRef);
    returnValue = TCM_Ok;
  }

  TCM_UnlockMasterMutex ( "TCM_Failure" );
  return returnValue;
}

TCM_Return_Type TCM_AddExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					STRING type,
					const TCM_Action_Ref &action,
					unsigned int maxInvocations,
					signed int index )
					 //= DEFAULT_EXCEPTION_HANDLER_PRIORITY
{
  TCM_LockMasterMutex ( "TCM_AddExceptionHandler" );

  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_AddExceptionHandler");

  addExceptionHandler(nodeRef, type, action, maxInvocations, index );

  TCM_UnlockMasterMutex ( "TCM_AddExceptionHandler" );
  return TCM_Ok;
}

TCM_Return_Type TCM_AddExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					STRING type,
					const CALLBACK_FN_TYPE callback,
					unsigned int maxInvocations)
{
  TCM_LockMasterMutex ( "TCM_AddExceptionHandler" );

  TCM_Return_Type returnValue
    = TCM_AddExceptionHandler(nodeRef, type, new Callback_Action(callback),
			      maxInvocations);

  TCM_UnlockMasterMutex ( "TCM_AddExceptionHandler" );
  return returnValue;
}

TCM_Return_Type TCM_RemoveExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					   STRING type,
					   const TCM_Action_Ref &action)
{
  TCM_LockMasterMutex ( "TCM_RemoveExceptionHandler" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_RemoveExceptionHandler");

  removeExceptionHandler(nodeRef, type, action);

  TCM_UnlockMasterMutex ( "TCM_RemoveExceptionHandler" );
  return TCM_Ok;
}

TCM_Return_Type TCM_RemoveExceptionHandler(const TCM_Task_Tree_Ref &nodeRef,
					   STRING type,
					   const CALLBACK_FN_TYPE callback)
{
  TCM_LockMasterMutex ( "TCM_RemoveExceptionHandler" );

  Callback_Action action(callback);

  TCM_Return_Type returnValue
    = TCM_RemoveExceptionHandler(nodeRef, type, &action);

  TCM_UnlockMasterMutex ( "TCM_RemoveExceptionHandler" );
  return returnValue;
}

TCM_Return_Type TCM_Bypass(const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_Bypass" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_Bypass");

  TCM_Return_Type  returnValue;

  if (nodeRef->isException()) {
    ((Exception_Node *)(*nodeRef))->bypass();
    returnValue = TCM_Ok;
  } else {
    tcmError("TCM_bypass: %s is not an exception node\n", 
	     nodeRef->instanceName());
    returnValue = TCM_Error;
  }

  TCM_UnlockMasterMutex ( "TCM_Bypass" );
  return returnValue;
}

TCM_Return_Type TCM_Retry(const TCM_Task_Tree_Ref &nodeRef,
			  const void *retryData)
{
  TCM_LockMasterMutex ( "TCM_Retry" );

  UNUSED(retryData);

  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_Retry");  

  tcmWarning("TCM_Retry: Not Yet Implemented\n");

  TCM_UnlockMasterMutex ( "TCM_Retry" );
  return TCM_Ok;
}

/* The failure data is now stored with the exception, which is part
   of Exception_Instance */
const void *TCM_FailureData (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_FailureData" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_FailureData", NULL);
	
  const void * returnValue = NULL;

  if (nodeRef->isException()) {
    returnValue =
      ((Exception_Node *)*nodeRef)->getException()->getExceptionData();
  } else {
    tcmWarning("Trying to get failure data from non-exception node %s\n",
	       nodeRef->getName());
  } 

  TCM_UnlockMasterMutex ( "TCM_FailureData" );
  return returnValue;
}

TCM_Exception *TCM_FailureException (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_FailureException" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_FailureException", NULL);
	
  TCM_Exception * returnValue = NULL;

  if (nodeRef->isException()) {
    returnValue = *((Exception_Node *)*nodeRef)->getException();
  } else {
    tcmWarning("Trying to get failure exception from non-exception node %s\n",
	       nodeRef->getName());
  } 

  TCM_UnlockMasterMutex ( "TCM_FailureException" );
  return returnValue;
}

/* Return the node that raised the exception */
TCM_Task_Tree_Ref TCM_FailureNode (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_FailureNode" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_FailureNode", NULL);
	
  TCM_Task_Tree_Ref returnValue = NULL;

  if (nodeRef->isException()) {
    returnValue = ((Exception_Node *)*nodeRef)->getFailureNode();
  } else {
    tcmWarning("Trying to get failure node from non-exception node %s\n",
	       nodeRef->getName());
  } 

  TCM_UnlockMasterMutex ( "TCM_FailureNode" );
  return returnValue;
}

/* Return the node associated with the exception handler */
TCM_Task_Tree_Ref TCM_ExceptionHandlerNode (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_ExceptionHandlerNode" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_ExceptionHandlerNode", NULL);
	
  TCM_Task_Tree_Ref returnValue = NULL;

  if (nodeRef->isException()) {
    returnValue = ((Exception_Node *)*nodeRef)->getContextNode();
  } else {
    tcmWarning("Trying to get exception handler node from non-exception node %s\n",
	       nodeRef->getName());
  } 

  TCM_UnlockMasterMutex ( "TCM_ExceptionHandlerNode" );
  return returnValue;
}

/*****************************************************************
 * 
 * Functions having to do with monitors
 *
 *****************************************************************/

static STRING monitorActivationName (
			       Task_Tree_Monitor_Node * theMonitor,
			       STRING                   theActivationTypeName,
			       char                   * theSpaceToWriteTheName,
			       unsigned int             theSizeOftheSpace )
{
  TCM_LockMasterMutex ( "monitorActivationName" );
  STRING  returnValue;

  if ( theActivationTypeName != STRING(NULL) )
  {
    returnValue = theActivationTypeName;
  }
  else
  {
    snprintf ( theSpaceToWriteTheName, theSizeOftheSpace,
	       "ACT-%s",
	       theMonitor -> instanceName() );
    returnValue = theSpaceToWriteTheName;
  }

  TCM_UnlockMasterMutex ( "monitorActivationName" );
  return returnValue;
}


/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_AllocateMonitorNode (STRING nodeTypeName)
 *
 * DESCRIPTION: Create a new monitor node, with the given name.
 *
 * NOTE: The node is not connected to the task tree, and is not runnable.
 *
 **************************************************************************/

TCM_Task_Tree_Ref TCM_AllocateMonitorNode (STRING nodeTypeName)
{
  TCM_LockMasterMutex ( "TCM_AllocateMonitorNode" );

  TCM_Task_Tree_Ref returnValue = new Task_Tree_Monitor_Node(nodeTypeName);

  TCM_UnlockMasterMutex ( "TCM_AllocateMonitorNode" );
  return returnValue;
}

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocateMonitorNode (STRING nodeTypeName,
					   STRING instanceName)
{
  TCM_LockMasterMutex ( "TCM_AllocateMonitorNode" );

  TCM_Task_Tree_Ref nodeRef = TCM_AllocateMonitorNode(nodeTypeName);
  TCM_SetInstanceName(nodeRef, instanceName);

  TCM_UnlockMasterMutex ( "TCM_AllocateMonitorNode" );
  return nodeRef;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_SetMonitorParameters (...)
 *
 * DESCRIPTION: Set all the parameters for the (regular) monitor.
 *
 * NOTE: The node is still not connected to the task tree, and is not runnable.
 *
 **************************************************************************/

TCM_Return_Type 
TCM_SetMonitorParameters (const TCM_Task_Tree_Ref &monitorRef,
			  const TCM_Action_Ref &activationAction,
			  unsigned int maxActivations,
			  unsigned int maxTriggers,
			  STRING activationTypeName,
			  int activationConstraints)
{
  TCM_LockMasterMutex ( "TCM_SetMonitorParameters" );

  TCM_Return_Type  returnValue;


  if (!monitorRef->isMonitor()) {
    tcmWarning("TCM_SetMonitorParameters: %s is not a monitor\n",
	       monitorRef->instanceName());
    returnValue = TCM_Error;
  } else {
    Task_Tree_Monitor_Node *monitor = ((Task_Tree_Monitor_Node *)*monitorRef);

#define TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH 255
    char spaceToWriteName [ 1 + TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH ];

    Monitor_Action *action
      = new Monitor_Action(
	      activationAction,
	      monitorActivationName(monitor, activationTypeName,
				    spaceToWriteName,
				    TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH),
	      maxActivations, activationConstraints);

#undef TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH

    monitor->setAction(action);
    monitor->setMaxTriggers(maxTriggers);

    returnValue = TCM_Ok;
  }


  TCM_UnlockMasterMutex ( "TCM_SetMonitorParameters" );
  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_AllocatePollingMonitorNode (STRING nodeTypeName)
 *
 * DESCRIPTION: Create a new polling monitor node, with the given name.
 *
 * NOTE: The node is not connected to the task tree, and is not runnable.
 *
 **************************************************************************/

TCM_Task_Tree_Ref TCM_AllocatePollingMonitorNode (STRING nodeTypeName)
{
  TCM_LockMasterMutex ( "TCM_AllocatePollingMonitorNode" );

  TCM_Task_Tree_Ref returnValue = new Task_Tree_Monitor_Node(nodeTypeName);

  TCM_UnlockMasterMutex ( "TCM_AllocatePollingMonitorNode" );
  return returnValue;
}

// Version where one can set the type and instance names separately
TCM_Task_Tree_Ref TCM_AllocatePollingMonitorNode (STRING nodeTypeName,
						  STRING instanceName)
{
  TCM_LockMasterMutex ( "TCM_AllocatePollingMonitorNode" );

  TCM_Task_Tree_Ref nodeRef = TCM_AllocatePollingMonitorNode(nodeTypeName);
  TCM_SetInstanceName(nodeRef, instanceName);

  TCM_UnlockMasterMutex ( "TCM_AllocatePollingMonitorNode" );
  return nodeRef;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_SetPollingMonitorParameters (...)
 *
 * DESCRIPTION: Set all the parameters for the (regular) monitor.
 *
 * NOTE: The node is still not connected to the task tree, and is not runnable.
 *
 **************************************************************************/

TCM_Return_Type 
TCM_SetPollingMonitorParameters (const TCM_Task_Tree_Ref &monitorRef,
				 const TCM_Action_Ref &activationAction,
				 MSecs period,
				 unsigned int maxActivations,
				 unsigned int maxTriggers,
				 STRING activationTypeName,
				 int activationConstraints,
				 BOOLEAN initialWait)
{
  TCM_LockMasterMutex ( "TCM_SetPollingMonitorParameters" );

  TCM_Return_Type  returnValue;

  if (!monitorRef->isMonitor()) {
    tcmWarning("TCM_SetPollingMonitorParameters: %s is not a monitor\n",
	       monitorRef->instanceName());
    returnValue = TCM_Error;
  } else {
    Task_Tree_Monitor_Node *monitor = ((Task_Tree_Monitor_Node *)*monitorRef);

#define TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH 255
    char spaceToWriteName [ 1 + TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH ];

    Polling_Monitor_Action *action 
      = new Polling_Monitor_Action(
	      activationAction,
	      monitorActivationName(monitor, activationTypeName,
				    spaceToWriteName,
				    TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH),
	      period, maxActivations,
	      activationConstraints, initialWait);

#undef TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH

    monitor->setAction(action);
    monitor->setMaxTriggers(maxTriggers);

    returnValue = TCM_Ok;
  }

  TCM_UnlockMasterMutex ( "TCM_SetPollingMonitorParameters" );
  return returnValue;
}

TCM_Task_Tree_Ref TCM_CreateMonitorNode (const TCM_Task_Tree_Ref &parentRef,
					 STRING nodeTypeName, 
					 const TCM_Action_Ref &activationAction,
					 unsigned int maxActivations,
					 unsigned int maxTriggers,
					 STRING activationTypeName,
					 int activationConstraints)
{
  TCM_LockMasterMutex ( "TCM_CreateMonitorNode" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_CreateMonitorNode", NullRef);

  TCM_Task_Tree_Ref monitorRef = TCM_AllocateMonitorNode(nodeTypeName);

  TCM_SetMonitorParameters(monitorRef, activationAction, maxActivations,
			   maxTriggers, activationTypeName, 
			   activationConstraints);

  taskTreeInsert(*parentRef, *monitorRef, TRUE);

  TCM_UnlockMasterMutex ( "TCM_CreateMonitorNode" );
  return monitorRef;
}

TCM_Task_Tree_Ref TCM_CreateMonitorNode (const TCM_Task_Tree_Ref &parentRef,
					 STRING nodeTypeName, 
					 const void *callbackData,
					 CALLBACK_FN_TYPE activationCallback,
					 unsigned int maxActivations,
					 unsigned int maxTriggers,
					 STRING activationTypeName,
					 int activationConstraints)
{
  TCM_LockMasterMutex ( "TCM_CreateMonitorNode" );

  TCM_Task_Tree_Ref returnValue
    = TCM_CreateMonitorNode(parentRef, nodeTypeName,
			    new Callback_Action(activationCallback,
						callbackData),
			    maxActivations, maxTriggers,
			    activationTypeName, activationConstraints);

  TCM_UnlockMasterMutex ( "TCM_CreateMonitorNode" );
  return returnValue;
}

TCM_Task_Tree_Ref
TCM_CreatePollingMonitorNode (const TCM_Task_Tree_Ref &parentRef,
			      STRING nodeTypeName,
			      const TCM_Action_Ref &activationAction,
			      MSecs period,
			      unsigned int maxActivations,
			      unsigned int maxTriggers,
			      STRING activationTypeName,
			      int activationConstraints,
			      BOOLEAN initialWait)
{
  TCM_LockMasterMutex ( "TCM_CreatePollingMonitorNode" );
  RETURN_UNLOCK_ON_INVALID_NODE(parentRef, "TCM_CreatePollingMonitorNode",
				NullRef);

  TCM_Task_Tree_Ref monitorRef = TCM_AllocateMonitorNode(nodeTypeName);

  TCM_SetPollingMonitorParameters(monitorRef, activationAction, period,
				  maxActivations, maxTriggers, 
				  activationTypeName,
				  activationConstraints, initialWait);

  taskTreeInsert(*parentRef, *monitorRef, TRUE);

  TCM_UnlockMasterMutex ( "TCM_CreatePollingMonitorNode" );
  return monitorRef;
}

TCM_Task_Tree_Ref
TCM_CreatePollingMonitorNode (const TCM_Task_Tree_Ref &parentRef,
			      STRING nodeTypeName,
			      const void *callbackData,
			      CALLBACK_FN_TYPE activationCallback,
			      MSecs period,
			      unsigned int maxActivations,
			      unsigned int maxTriggers,
			      STRING activationTypeName,
			      int activationConstraints,
			      BOOLEAN initialWait)
{
  TCM_LockMasterMutex ( "TCM_CreatePollingMonitorNode" );

  TCM_Task_Tree_Ref returnValue
    = TCM_CreatePollingMonitorNode(parentRef, nodeTypeName,
				   new Callback_Action(activationCallback,
						       callbackData),
				   period, maxActivations, maxTriggers,
				   activationTypeName, activationConstraints,
				   initialWait);

  TCM_UnlockMasterMutex ( "TCM_CreatePollingMonitorNode" );
  return returnValue;
}



/**************************************************************************
 *
 * FUNCTION: TCM_Task_Tree_Ref TCM_AllocateCompleteMonitorNode ( ... )
 *
 * DESCRIPTION: Creates a Monitor_Node with an attached (defaulted)
 *              Polling_Monitor_Action that is configured for non-polling.
 *
 * NOTE: TDL utilizes a mechanism that requires independent allocation,
 *       set-constaints, and set-action functions.  This is here to fulfill
 *       TDL's interface requirements...
 *
 * NOTE: Use TCM_SetActualAction to set this Monitor's activation-action.
 *
 **************************************************************************/

TCM_Task_Tree_Ref
TCM_AllocateCompleteMonitorNode (
			STRING       theNodeTypeName,
			STRING       theInstanceName,         //= STRING(NULL)
			int          theActivationConstraints,//= 0
			unsigned int theMaximumActivations,   //= MAXINT
			unsigned int theMaximumTriggers,      //= MAXINT
			MSecs        thePeriod,               //= INFINITE_TIME
			BOOLEAN      theInitialWait,          //= FALSE
			STRING       theActivationTypeName )  //= STRING(NULL)
{
  TCM_LockMasterMutex ( "TCM_AllocateCompleteMonitorNode" );

  TCM_Task_Tree_Ref         monitorNode;
  Polling_Monitor_Action  * monitorAction;

#define TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH 255
  char     spaceToWriteName [ 1 + TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH ];

  if ( theInstanceName == STRING(NULL) )
    monitorNode = TCM_AllocateMonitorNode ( theNodeTypeName );
  else
    monitorNode = TCM_AllocateMonitorNode ( theNodeTypeName, theInstanceName );

  theActivationTypeName
    = monitorActivationName ( ( (Task_Tree_Monitor_Node *)
				(monitorNode . operator*()) ),
			      theActivationTypeName,
			      spaceToWriteName,
			      TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH );

#undef TCM_SPACE_TO_WRITE_MAXIMUM_LENGTH



  monitorAction = new Polling_Monitor_Action ( theActivationTypeName,
					       theActivationConstraints,
					       theMaximumActivations,
					       thePeriod,
					       theInitialWait );
  TCM_SetAction ( monitorNode, monitorAction );
  TCM_SetMonitorMaximumTriggers ( monitorNode, theMaximumTriggers );

  TCM_UnlockMasterMutex ( "TCM_AllocateCompleteMonitorNode" );
  return monitorNode;
}


TCM_Return_Type
TCM_SetMonitorMaximumActivations ( const TCM_Task_Tree_Ref & theMonitorRef,
				   unsigned int theMaximumActivates )
{
  TCM_LockMasterMutex ( "TCM_SetMonitorMaximumActivations" );

  Task_Tree_Monitor_Node * monitor;

  RETURN_UNLOCK_IF_INVALID_MONITOR ( theMonitorRef,
				     "TCM_SetMonitorMaximumActivations",
				     TRUE  /* checkAction  */,
				     FALSE /* checkPolling */,
				     monitor );

  ( (Monitor_Action *) (TCM_GetAction ( monitor ) . operator*()) )
    -> setMaximumActivations ( theMaximumActivates );

  TCM_UnlockMasterMutex ( "TCM_SetMonitorMaximumActivations" );
  return TCM_Ok;
}

unsigned int
TCM_GetMonitorMaximumActivations ( const TCM_Task_Tree_Ref & theMonitorRef )
{
  TCM_LockMasterMutex ( "TCM_GetMonitorMaximumActivations" );

  Task_Tree_Monitor_Node * monitor;
  unsigned int             returnValue;

  if ( checkMonitorValidity( theMonitorRef,
			     "TCM_GetMonitorMaximumActivations",
			     TRUE /* checkAction */, FALSE /*checkPolling*/,
			     monitor ) )
    returnValue
      = ( (Monitor_Action *) (TCM_GetAction ( monitor ) . operator*()) )
          -> getMaximumActivations();
  else
    returnValue = 0;

  TCM_UnlockMasterMutex ( "TCM_GetMonitorMaximumActivations" );
  return returnValue;
}

TCM_Return_Type
TCM_SetMonitorMaximumTriggers ( const TCM_Task_Tree_Ref & theMonitorRef,
				unsigned int theMaximumTriggers )
{
  TCM_LockMasterMutex ( "TCM_SetMonitorMaximumTriggers" );

  Task_Tree_Monitor_Node * monitor;

  RETURN_UNLOCK_IF_INVALID_MONITOR ( theMonitorRef,
				     "TCM_SetMonitorMaximumTriggers",
				     FALSE /* checkAction  */,
				     FALSE /* checkPolling */,
				     monitor );

  monitor -> setMaxTriggers ( theMaximumTriggers );

  TCM_UnlockMasterMutex ( "TCM_SetMonitorMaximumTriggers" );
  return TCM_Ok;
}

unsigned int
TCM_GetMonitorMaximumTriggers ( const TCM_Task_Tree_Ref & theMonitorRef )
{
  TCM_LockMasterMutex ( "TCM_GetMonitorMaximumTriggers" );

  Task_Tree_Monitor_Node * monitor;
  unsigned int             returnValue;

  if ( checkMonitorValidity( theMonitorRef, "TCM_GetMonitorMaximumTriggers",
			     FALSE /* checkAction */, FALSE /*checkPolling*/,
			     monitor ) )
    returnValue = monitor -> getMaxTriggers ();
  else
    returnValue = 0;

  TCM_UnlockMasterMutex ( "TCM_GetMonitorMaximumTriggers" );
  return returnValue;
}

TCM_Return_Type
TCM_SetMonitorActivationConstraints ( const TCM_Task_Tree_Ref & theMonitorRef,
				      int theActivationConstraints )
{
  TCM_LockMasterMutex ( "TCM_SetMonitorActivationConstraints" );
  Task_Tree_Monitor_Node * monitor;

  RETURN_UNLOCK_IF_INVALID_MONITOR ( theMonitorRef,
				     "TCM_SetMonitorActivationConstraints",
				     TRUE  /* checkAction  */,
				     FALSE /* checkPolling */,
				     monitor );

  ( (Monitor_Action *) (TCM_GetAction ( monitor ) . operator*()) )
    -> setActivationConstraints ( theActivationConstraints );

  TCM_UnlockMasterMutex ( "TCM_SetMonitorActivationConstraints" );
  return TCM_Ok;
}

int
TCM_GetMonitorActivationConstraints( const TCM_Task_Tree_Ref & theMonitorRef )
{
  TCM_LockMasterMutex ( "TCM_GetMonitorActivationConstraints" );

  Task_Tree_Monitor_Node * monitor;
  int                      returnValue;

  if ( checkMonitorValidity( theMonitorRef,
			     "TCM_GetMonitorActivationConstraints",
			     TRUE /* checkAction */, FALSE /*checkPolling*/,
			     monitor ) )
    returnValue
      = ( (Monitor_Action *) (TCM_GetAction ( monitor ) . operator*()) )
          -> getActivationConstraints();
  else
    returnValue = 0;

  TCM_UnlockMasterMutex ( "TCM_GetMonitorActivationConstraints" );
  return returnValue;
}

TCM_Return_Type
TCM_SetMonitorPeriod ( const TCM_Task_Tree_Ref & theMonitorRef,
		       MSecs thePeriod )
{
  TCM_LockMasterMutex ( "TCM_SetMonitorPeriod" );

  Task_Tree_Monitor_Node * monitor;

  RETURN_UNLOCK_IF_INVALID_MONITOR ( theMonitorRef, "TCM_SetMonitorPeriod",
				     TRUE /* checkAction  */,
				     TRUE /* checkPolling */,
				     monitor  );

  ( (Polling_Monitor_Action *) (TCM_GetAction ( monitor ) . operator*()) )
    -> setPeriod ( thePeriod );

  TCM_UnlockMasterMutex ( "TCM_SetMonitorPeriod" );
  return TCM_Ok;
}


MSecs
TCM_GetMonitorPeriod ( const TCM_Task_Tree_Ref & theMonitorRef )
{
  TCM_LockMasterMutex ( "TCM_GetMonitorPeriod" );

  Task_Tree_Monitor_Node * monitor;
  MSecs                    returnValue;

  if ( checkMonitorValidity( theMonitorRef, "TCM_GetMonitorPeriod",
			     TRUE /* checkAction */, TRUE /*checkPolling*/,
			     monitor ) )
    returnValue = ((Polling_Monitor_Action *)
		   (TCM_GetAction ( monitor ) . operator*()) ) -> getPeriod();
  else
    returnValue = INFINITE_TIME;

  TCM_UnlockMasterMutex ( "TCM_GetMonitorPeriod" );
  return returnValue;
}


TCM_Return_Type
TCM_SetMonitorInitialWait ( const TCM_Task_Tree_Ref & theMonitorRef,
			    BOOLEAN theInitialWait )
{
  TCM_LockMasterMutex ( "TCM_SetMonitorInitialWait" );

  Task_Tree_Monitor_Node * monitor;

  RETURN_UNLOCK_IF_INVALID_MONITOR ( theMonitorRef,
				     "TCM_SetMonitorInitialWait",
				     TRUE /* checkAction  */,
				     TRUE /* checkPolling */,
				     monitor );

  ( (Polling_Monitor_Action *) (TCM_GetAction ( monitor ) . operator*()) )
    -> setInitialWait ( theInitialWait );

  TCM_UnlockMasterMutex ( "TCM_SetMonitorInitialWait" );
  return TCM_Ok;
}

BOOLEAN
TCM_GetMonitorInitialWait ( const TCM_Task_Tree_Ref & theMonitorRef )
{
  TCM_LockMasterMutex ( "TCM_GetMonitorInitialWait" );

  Task_Tree_Monitor_Node * monitor;
  BOOLEAN                  returnValue;

  if ( checkMonitorValidity( theMonitorRef, "TCM_GetMonitorInitialWait",
			     TRUE /* checkAction */, TRUE /*checkPolling*/,
			     monitor ) )
    returnValue
      = ((Polling_Monitor_Action *)
	 (TCM_GetAction ( monitor ) . operator*()) ) -> getInitialWait();
  else
    returnValue = FALSE;

  TCM_UnlockMasterMutex ( "TCM_GetMonitorInitialWait" );
  return returnValue;
}




TCM_Return_Type TCM_Trigger (const TCM_Task_Tree_Ref &monitorRef)
{
  TCM_LockMasterMutex ( "TCM_Trigger" );
  RETURN_UNLOCK_IF_INVALID_NODE(monitorRef, "TCM_Trigger");

  Task_Tree_Monitor_Node * monitor = monitorDereference(monitorRef);
  TCM_Return_Type          returnValue;

  if (monitor) {
    monitor->trigger();
    returnValue = TCM_Ok;
  } else {
    tcmWarning("TCM_Trigger: Node is not a monitor\n");
    returnValue = TCM_Error;
  }

  TCM_UnlockMasterMutex ( "TCM_Trigger" );
  return returnValue;
}

static void doActivation (const void *data, const Task_Tree_Ref &monitorRef)
{
  TCM_LockMasterMutex ( "doActivation" );

  Task_Tree_Monitor_Node *monitor = monitorDereference(monitorRef);

  monitor->activate(data);

  TCM_UnlockMasterMutex ( "doActivation" );
}

TCM_Return_Type TCM_Activate (const TCM_Task_Tree_Ref &monitorRef,
			      const void *activationData)
{
  TCM_LockMasterMutex ( "TCM_Activate" );
  RETURN_UNLOCK_IF_INVALID_NODE(monitorRef, "TCM_Activate");

  Task_Tree_Monitor_Node * monitor = monitorDereference(monitorRef);
  TCM_Return_Type          returnValue;

  if (monitor) {
    if (monitor->isCurrentState(Handling_State)) {
      monitor->activate(activationData);
    } else {
      // Delay until current signals are handled -- monitor hasn't started yet.
      Event activateEvent(doActivation, activationData);
      activateEvent.sendSignal(monitorRef);
    }
    returnValue = TCM_Ok;
  } else {
    tcmWarning("TCM_Activate: Node is not a monitor\n");
    returnValue = TCM_Error;
  }

  TCM_UnlockMasterMutex ( "TCM_Activate" );
  return returnValue;
}

TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				TCM_Point const &activateTimePoint,
				const void *activationData)
{
  TCM_LockMasterMutex ( "TCM_ActivateAt" );
  RETURN_UNLOCK_IF_INVALID_NODE(monitorRef, "TCM_ActivateAt");

  Task_Tree_Node * monitor = monitorDereference(monitorRef);
  TCM_Return_Type  returnValue;

  if (monitor) {
    Time_Point point(activateTimePoint);
    addEventPair(point.nodeOf(), point.stateOf(), Activate_Signal, monitor,
		 activationData);
    returnValue = TCM_Ok;
  } else {
    tcmWarning("TCM_ActivateAt: Node is not a monitor\n");
    returnValue = TCM_Error;
  }

  TCM_UnlockMasterMutex ( "TCM_ActivateAt" );
  return returnValue;
}

TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				MSecs absoluteTime,
				const void *activationData)
{
  TCM_LockMasterMutex ( "TCM_ActivateAt" );
  RETURN_UNLOCK_IF_INVALID_NODE(monitorRef, "TCM_ActivateAt");

  Task_Tree_Node * monitor = monitorDereference(monitorRef);
  TCM_Return_Type  returnValue;

  if (monitor) {
    addTimedEvent(absoluteTime, monitor, Activate_Signal, activationData);
    returnValue = TCM_Ok;
  } else {
    tcmWarning("TCM_ActivateAt: Node is not a monitor\n");
    returnValue = TCM_Error;
  }

  TCM_UnlockMasterMutex ( "TCM_ActivateAt" );
  return returnValue;
}

TCM_Return_Type TCM_ActivateIn (const TCM_Task_Tree_Ref &monitorRef,
				MSecs relativeWait,
				const void *activationData)
{
  TCM_LockMasterMutex ( "TCM_ActivateIn" );
  RETURN_UNLOCK_IF_INVALID_NODE(monitorRef, "TCM_ActivateIn");

  Task_Tree_Node * monitor = monitorDereference(monitorRef);
  TCM_Return_Type  returnValue;

  if (monitor) {
    returnValue
      = TCM_ActivateAt(monitor, absoluteTime(relativeWait), activationData);
  } else {
    tcmWarning("TCM_ActivateIn: Node is not a monitor\n");
    returnValue = TCM_Error;
  }

  TCM_UnlockMasterMutex ( "TCM_ActivateIn" );
  return returnValue;
}

// Activate this node in "relativeWait" msecs after 
//  the "activateTimePoint" have passed.
TCM_Return_Type TCM_ActivateInAfter (const TCM_Task_Tree_Ref &monitorRef,
				     MSecs relativeWait,
				     TCM_Point const &activateTimePoint,
				     const void *activationData)
{
  TCM_LockMasterMutex ( "TCM_ActivateInAfter" );
  RETURN_UNLOCK_IF_INVALID_NODE(monitorRef, "TCM_ActivateInAfter");
  RETURN_UNLOCK_IF_INVALID_TIME(activateTimePoint, "TCM_ActivateInAfter");

  Task_Tree_Node * monitor = monitorDereference(monitorRef);
  Time_Point       point(activateTimePoint);
  TCM_Return_Type  returnValue;

  if (monitor) {
    constrainWhenAfter(monitor, Activate_Signal, point, relativeWait,
		       activationData);
    returnValue = TCM_Ok;
  } else {
    tcmWarning("TCM_ActivateInAfter: Node is not a monitor\n");
    returnValue = TCM_Error;
  }

  TCM_UnlockMasterMutex ( "TCM_ActivateInAfter" );
  return returnValue;
}

/* For backwards compatibility */
TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				TCM_Point const &activateTimePoint)
{
  TCM_LockMasterMutex ( "TCM_ActivateAt" );

  TCM_Return_Type returnValue
    = TCM_ActivateAt(monitorRef, activateTimePoint, NULL);

  TCM_UnlockMasterMutex ( "TCM_ActivateAt" );
  return returnValue;
}

TCM_Return_Type TCM_ActivateAt (const TCM_Task_Tree_Ref &monitorRef,
				MSecs absoluteTime)
{
  TCM_LockMasterMutex ( "TCM_ActivateAt" );

  TCM_Return_Type returnValue
    = TCM_ActivateAt(monitorRef, absoluteTime, NULL);

  TCM_UnlockMasterMutex ( "TCM_ActivateAt" );
  return returnValue;
}

TCM_Return_Type TCM_ActivateIn (const TCM_Task_Tree_Ref &monitorRef,
				MSecs relativeWait)
{
  TCM_LockMasterMutex ( "TCM_ActivateIn" );

  TCM_Return_Type returnValue
    = TCM_ActivateIn(monitorRef, relativeWait, NULL);

  TCM_UnlockMasterMutex ( "TCM_ActivateIn" );
  return returnValue;
}


// Activate this node in "relativeWait" msecs after 
//  the "activateTimePoint" have passed.
TCM_Return_Type TCM_ActivateInAfter (const TCM_Task_Tree_Ref &monitorRef,
				     MSecs relativeWait,
				     TCM_Point const &activateTimePoint)
{
  TCM_LockMasterMutex ( "TCM_ActivateInAfter" );

  TCM_Return_Type returnValue
    = TCM_ActivateInAfter ( monitorRef, relativeWait,
			    activateTimePoint, NULL  );

  TCM_UnlockMasterMutex ( "TCM_ActivateInAfter" );
  return returnValue;
}


const void *TCM_ActivationData (const TCM_Task_Tree_Ref &monitorRef)
{
  TCM_LockMasterMutex ( "TCM_ActivationData" );
  RETURN_UNLOCK_ON_INVALID_NODE(monitorRef, "TCM_ActivationData", NULL);

  const void * returnValue;

  if (monitorDereference(monitorRef)) {
    // Want to check if it is a monitor, but use the node data on 
    // the individual node (in case multiple activations are queued).
    returnValue = monitorRef->getNodeData();
  } else {
    tcmWarning("TCM_ActivationData: Node is not a monitor\n");
    returnValue = NULL;
  }

  TCM_UnlockMasterMutex ( "TCM_ActivationData" );
  return returnValue;
}

unsigned int TCM_NumTriggers (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_NumTriggers" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_NumTriggers", 0);

  Task_Tree_Monitor_Node * monitor = monitorDereference(nodeRef);
  unsigned int             returnValue;

  if (monitor) {
    returnValue = monitor->numTriggers();
  } else {
    tcmWarning("TCM_NumTriggers: Node is not a monitor\n");
    returnValue = 0;
  }

  TCM_UnlockMasterMutex ( "TCM_NumTriggers" );
  return returnValue;
}

unsigned int TCM_NumActivations (const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_NumActivations" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_NumActivations", 0);

  Task_Tree_Monitor_Node * monitor = monitorDereference(nodeRef);
  unsigned int             returnValue;

  if (monitor) {
    returnValue = monitor->numActivations();
  } else {
    tcmWarning("TCM_NumActivations: Node is not a monitor\n");
    returnValue = 0;
  }

  TCM_UnlockMasterMutex ( "TCM_NumActivations" );
  return returnValue;
}


/*****************************************************************
 * 
 * Functions to invoke functions (not adding tree nodes) in 
 * response to particular events.
 *
 *****************************************************************/

TCM_Return_Type TCM_InvokeWhen (TCM_Point const &timePoint,
				SIMPLE_CALLBACK_FN_TYPE callback,
				const void *callbackData)
{
  TCM_LockMasterMutex ( "TCM_InvokeWhen" );
  RETURN_UNLOCK_IF_INVALID_TIME(timePoint, "TCM_InvokeWhen");
  RETURN_UNLOCK_IF_NULL(callback, "TCM_InvokeWhen");
  
  Time_Point point(timePoint);
  point.nodeOf()->addRequestedEvent(point.stateOf(), callback, callbackData);

  TCM_UnlockMasterMutex ( "TCM_InvokeWhen" );
  return TCM_Ok;
}

TCM_Return_Type TCM_InvokeWhen (MSecs absoluteTime,
				SIMPLE_CALLBACK_FN_TYPE callback,
				const void *callbackData)
{
  TCM_LockMasterMutex ( "TCM_InvokeWhen" );
  RETURN_UNLOCK_IF_NULL(callback, "TCM_InvokeWhen");
  
  addTimedEvent(absoluteTime, callback, callbackData);

  TCM_UnlockMasterMutex ( "TCM_InvokeWhen" );
  return TCM_Ok;
}

TCM_Return_Type TCM_InvokeAfter (MSecs relativeWait,
				 SIMPLE_CALLBACK_FN_TYPE callback,
				 const void *callbackData)
{
  TCM_LockMasterMutex ( "TCM_InvokeAfter" );
  RETURN_UNLOCK_IF_NULL(callback, "TCM_InvokeAfter");

  TCM_Return_Type returnValue
    = TCM_InvokeWhen(absoluteTime(relativeWait), callback, callbackData);

  TCM_UnlockMasterMutex ( "TCM_InvokeAfter" );
  return returnValue;
}

TCM_Return_Type TCM_InvokeWhenTerminating (const TCM_Task_Tree_Ref &nodeRef,
					   SIMPLE_CALLBACK_FN_TYPE callback,
					   const void *callbackData)
{
  TCM_LockMasterMutex ( "TCM_InvokeWhenTerminating" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_InvokeWhenTerminating");
  RETURN_UNLOCK_IF_NULL(callback, "TCM_InvokeWhenTerminating");
  
  nodeRef->addRequestedEvent(Terminating_State, callback, callbackData);

  TCM_UnlockMasterMutex ( "TCM_InvokeWhenTerminating" );
  return TCM_Ok;
}

TCM_Return_Type TCM_InvokeWhenTerminated (const TCM_Task_Tree_Ref &nodeRef,
					  SIMPLE_CALLBACK_FN_TYPE callback,
					  const void *callbackData)
{
  TCM_LockMasterMutex ( "TCM_InvokeWhenTerminated" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_InvokeWhenTerminated");
  RETURN_UNLOCK_IF_NULL(callback, "TCM_InvokeWhenTerminated");
  
  nodeRef->addRequestedEvent(Terminated_State, callback, callbackData);

  TCM_UnlockMasterMutex ( "TCM_InvokeWhenTerminated" );
  return TCM_Ok;
}

/*****************************************************************
 * 
 * Functions for interacting with TCM
 *
 *****************************************************************/

TCM_Return_Type TCM_Initialize (void)
{
  TCM_LockMasterMutex ( "TCM_Initialize" );

  initializeGlobalState();

  tcmStartTerminalLogging();

  TCM_UnlockMasterMutex ( "TCM_Initialize" );
  return TCM_Ok;
}

/**************************************************************************
 *
 * FUNCTION: TCM_Return_Type TCM_ProcessAgenda(...)
 *
 * DESCRIPTION:
 *
 *   If "waitingAllowed" is TRUE, when TCM_ProcessAgenda() has finished
 * its work, (empty _taskQueue, empty _eventQueue), it will invoke
 * _externalEvents.dispatchEvents(...) with a timeout based upon
 * the _timerQueue, HasThreadWithUnfinishedWork(), and clamped
 * appropriately to prevent indefinite blocking.
 *   Of course, _externalEvents.dispatchEvents(...) can process input at
 * any time, and return early before this timeout period fully elapses.
 * Thus permitting TCM_ProcessAgenda() (actually Agenda::clearQueues())
 * to resume processing early on any newly created tasks.
 *   (Addendum: _externalEvents.dispatchEvents(...) is additionally invoked
 * with a timeout of zero-time to prevent starvation, irregardless of the
 * "waitingAllowed" value.)
 *
 *   If "waitingAllowed" is FALSE, TCM_ProcessAgenda() will spin-loop
 * without any blocking mechanism, potentially using up large quantities
 * of CPU time while it waits for timer-events to expire.
 *
 *   If "returnWhenAllWorkIsDone" is TRUE, TCM_ProcessAgenda() will return
 * when (1) the relativeTimeout time-period has elapsed, OR (2) as soon as
 * all work is completed.  (E.g.: _taskQueue, _eventQueue, and _timerQueue
 * are all empty.  HasThreadWithUnfinishedWork() is FALSE.)
 *   (Addendum: For the purposes of deciding whether or not work still remains
 * to be completed, we do **NOT** consider possible future incoming tasks from
 * _externalEvents.dispatchEvents(...).  E.g: Possible future incoming
 * Distributed tasks.)
 *
 *   If "returnWhenAllWorkIsDone" is FALSE, TCM_ProcessAgenda() will ONLY
 * return when the relativeTimeout time-period has elapsed.
 *
 *   If "silentlyAutoCorrectArguments" is TRUE,
 *   AND "waitingAllowed" is *NOT* equal to TRUE or FALSE,
 *   AND "relativeTimeout" *IS* equal to TRUE or FALSE,
 * the values for "waitingAllowed" and "relativeTimeout" will be swapped.
 *   This circumvents an ongoing issue wherein  "BOOLEAN", "MSecs", and
 * the "bool" type either all are, or autopromote to, integer types.
 * Thereby allowing these arguments to be trivially swapped, and preventing
 * us from addressing this via static compile-time type-checking signatures.
 *
 **************************************************************************/

TCM_Return_Type TCM_ProcessAgenda (
		 BOOLEAN waitingAllowed,                /* //= TRUE          */
		 MSecs   relativeTimeout,               /* //= INFINITE_TIME */
		 BOOLEAN returnWhenAllWorkIsDone,       /* = FALSE           */
		 BOOLEAN silentlyAutoCorrectArguments ) /* = TRUE            */
{
  TCM_LockMasterMutex ( "TCM_ProcessAgenda" );

	/* Not perfect, since we lose precision.  But how often will */
	/* relativeTimeout exceed 49.71 days?                        */
  if (   ( silentlyAutoCorrectArguments == TRUE )
      && (   ( waitingAllowed  != TRUE  )
	  || ( waitingAllowed  != FALSE ) )
      && (   ( relativeTimeout == TRUE  )
	  || ( relativeTimeout == FALSE ) ) )
  {
    tcmWarning("TCM_ProcessAgenda("
	       MSECS_PRINTF_STRING ",%s,%s,%s):  Auto-correcting:  "
	       "Assuming you meant TCM_ProcessAgenda(%s,"
	       MSECS_PRINTF_STRING ",%s,%s)\n",
#define TCM_TMP_PRINTF_BOOLEAN_VALUE(X) (((X)==FALSE)?"FALSE":"TRUE")
	       (MSecs)waitingAllowed,
	       TCM_TMP_PRINTF_BOOLEAN_VALUE(relativeTimeout),
	       TCM_TMP_PRINTF_BOOLEAN_VALUE(returnWhenAllWorkIsDone),
	       TCM_TMP_PRINTF_BOOLEAN_VALUE(silentlyAutoCorrectArguments),

	       TCM_TMP_PRINTF_BOOLEAN_VALUE(relativeTimeout),
	       (MSecs)waitingAllowed,
	       TCM_TMP_PRINTF_BOOLEAN_VALUE(returnWhenAllWorkIsDone),
	       TCM_TMP_PRINTF_BOOLEAN_VALUE(silentlyAutoCorrectArguments)
#undef TCM_TMP_PRINTF_BOOLEAN_VALUE
	       );

    MSecs tmpValue  = (MSecs) waitingAllowed;
    waitingAllowed  = relativeTimeout;
    relativeTimeout = tmpValue;
  }


  TCM_Return_Type  returnValue
    = ( GET_TCM_GLOBAL(agenda).clearQueues ( NULL,
					     absoluteTime ( relativeTimeout ),
					     waitingAllowed,
					     returnWhenAllWorkIsDone )
	== TRUE ) ? TCM_Ok : TCM_Error;

  TCM_UnlockMasterMutex ( "TCM_ProcessAgenda" );

  return returnValue;
}

TCM_Return_Type TCM_AddExternalEvent (int sd,
				      SD_CALLBACK_FN_TYPE callback,
				      const void *callbackData)
{
  TCM_LockMasterMutex ( "TCM_AddExternalEvent" );

  if (sd >= 0) {
    GET_TCM_GLOBAL(agenda).addExternalEvent(sd, callback, callbackData);
    TCM_UnlockMasterMutex ( "TCM_AddExternalEvent" );
    return TCM_Ok;
  } else {
    tcmError("TCM_AddExternalEvent: Illegal file descriptor %d\n", sd);
    TCM_UnlockMasterMutex ( "TCM_AddExternalEvent" );
    return TCM_Error;
  }
}
	
TCM_Return_Type TCM_RemoveExternalEvent (int sd)
{
  TCM_LockMasterMutex ( "TCM_RemoveExternalEvent" );

  if (sd >= 0) {
    GET_TCM_GLOBAL(agenda).removeExternalEvent(sd);
    TCM_UnlockMasterMutex ( "TCM_RemoveExternalEvent" );
    return TCM_Ok;
  } else {
    tcmError("TCM_RemoveExternalEvent: Illegal file descriptor %d\n", sd);
    TCM_UnlockMasterMutex ( "TCM_RemoveExternalEvent" );
    return TCM_Error;
  }
}

void TCM_SetExternalEventBypassHandler (
			 EXTERNAL_EVENT_BYPASS_HANDLER_TYPE theBypassHandler )
{
  TCM_LockMasterMutex ( "TCM_SetExternalEventBypassHandler" );

  GET_TCM_GLOBAL(agenda).setExternalEventBypassHandler ( theBypassHandler );

  TCM_UnlockMasterMutex ( "TCM_SetExternalEventBypassHandler" );
}


TCM_Return_Type TCM_DisplayTree(FILE *stream, const TCM_Task_Tree_Ref &nodeRef)
{
  TCM_LockMasterMutex ( "TCM_DisplayTree" );
  RETURN_UNLOCK_IF_INVALID_NODE(nodeRef, "TCM_DisplayTree");

  nodeRef->displayTree(stream, 0);

  TCM_UnlockMasterMutex ( "TCM_DisplayTree" );
  return TCM_Ok;
}

static void setLoggingOptions(Logger &logger, int options)
{
  TCM_LockMasterMutex ( "setLoggingOptions" );

  logger.setMessageLogging(options & Log_Msg);
  logger.setStatusLogging(options & Log_Status);
  logger.setTimeLogging(options & Log_Time);
  logger.setIdLogging(options & Log_Id);
  logger.setParentIdLogging(options & Log_ParentId);
  logger.setIgnoreLogging(options & Log_Ignore);

  TCM_UnlockMasterMutex ( "setLoggingOptions" );
}

static int getLoggingOptions(Logger &logger)
{
  TCM_LockMasterMutex ( "getLoggingOptions" );

  int options = Log_None;

  if (logger.message()) options |= Log_Msg;
  if (logger.status()) options |= Log_Status;
  if (logger.time()) options |= Log_Time;
  if (logger.id()) options |= Log_Id;
  if (logger.parentId()) options |= Log_ParentId;
  if (logger.ignore()) options |= Log_Ignore;

  TCM_UnlockMasterMutex ( "getLoggingOptions" );
  return options;
}

void TCM_SetTerminalLoggingOptions (int options)
{
  TCM_LockMasterMutex ( "TCM_SetTerminalLoggingOptions" );

  setLoggingOptions(GET_TCM_GLOBAL(terminalLogger), options);

  TCM_UnlockMasterMutex ( "TCM_SetTerminalLoggingOptions" );
}

void TCM_SetFileLoggingOptions (int options)
{
  TCM_LockMasterMutex ( "TCM_SetFileLoggingOptions" );

  setLoggingOptions(GET_TCM_GLOBAL(fileLogger), options);

  TCM_UnlockMasterMutex ( "TCM_SetFileLoggingOptions" );
}

int TCM_TerminalLoggingOptions (void)
{
  TCM_LockMasterMutex ( "TCM_TerminalLoggingOptions" );

  int returnValue = getLoggingOptions(GET_TCM_GLOBAL(terminalLogger));

  TCM_UnlockMasterMutex ( "TCM_TerminalLoggingOptions" );
  return returnValue;
}

int TCM_FileLoggingOptions (void)
{
  TCM_LockMasterMutex ( "TCM_FileLoggingOptions" );

  int returnValue = getLoggingOptions(GET_TCM_GLOBAL(fileLogger));

  TCM_UnlockMasterMutex ( "TCM_FileLoggingOptions" );
  return returnValue;
}


BOOLEAN TCM_GetAllowInfiniteTimeouts()
{
  TCM_LockMasterMutex ( "TCM_GetAllowInfiniteTimeouts" );

  BOOLEAN returnValue = GET_TCM_GLOBAL(agenda).getAllowInfiniteTimeouts();

  TCM_UnlockMasterMutex ( "TCM_GetAllowInfiniteTimeouts" );
  return returnValue;
}

void TCM_SetAllowInfiniteTimeouts ( BOOLEAN theAllowValue /* = TRUE */ )
{
  TCM_LockMasterMutex ( "TCM_SetAllowInfiniteTimeouts" );

  GET_TCM_GLOBAL(agenda).setAllowInfiniteTimeouts ( theAllowValue );

  TCM_UnlockMasterMutex ( "TCM_SetAllowInfiniteTimeouts" );
}

struct timeval TCM_GetInfiniteTimeTimeout()
{
  TCM_LockMasterMutex ( "TCM_GetInfiniteTimeTimeout" );

  struct timeval returnValue = GET_TCM_GLOBAL(agenda).getInfiniteTimeTimeout();

  TCM_UnlockMasterMutex ( "TCM_GetInfiniteTimeTimeout" );
  return returnValue;
}

void TCM_SetInfiniteTimeTimeout ( struct timeval theTimeout )
{
  TCM_LockMasterMutex ( "TCM_SetInfiniteTimeTimeout" );

  GET_TCM_GLOBAL(agenda).setInfiniteTimeTimeout ( theTimeout );

  TCM_UnlockMasterMutex ( "TCM_SetInfiniteTimeTimeout" );
}



/**************************************************************************
 * 
 * DESCRIPTION: Tests if the nodeRef was created with
 *              TCM_AllocateDistributedNode()
 *
 **************************************************************************/
BOOLEAN TCM_IsDistributedNode ( const TCM_Task_Tree_Ref & nodeRef )
{
  TCM_LockMasterMutex ( "TCM_IsDistributedNode" );
  RETURN_UNLOCK_ON_INVALID_NODE(nodeRef, "TCM_IsDistributedNode", FALSE);

  BOOLEAN returnValue = nodeRef->isVirtual();

  TCM_UnlockMasterMutex ( "TCM_IsDistributedNode" );
  return returnValue;
}


/*****************************************************************************
 ***************************** THREADING SUPPORT *****************************
 *****************************************************************************/

#ifdef THREADED
#include "ThreadManager.h"
#endif /* THREADED */


BOOLEAN
TCM_LockMasterMutex ( const char * theErrorString /* = (const char *)NULL */ )
{
#ifdef THREADED
  return ThreadManager::getGlobalThreadManager()
           . lockMasterMutex ( theErrorString );
#else  /* THREADED */
  return TRUE;
#endif /* THREADED */
}

BOOLEAN
TCM_UnlockMasterMutex ( const char * theErrorString /* = (const char *)NULL*/ )
{
#ifdef THREADED
  return ThreadManager::getGlobalThreadManager()
           . unlockMasterMutex ( theErrorString );
#else  /* THREADED */
  return TRUE; 
#endif /* THREADED */
}


  /* Sometimes, we need to temporarily remove all the current locks while
   * we wait for some event to transpire, task to exectute, etc.
   * These two functions accomplish this.
   */
unsigned int TCM_FullyUnlockMasterMutexReturningNumberOfLocks (
		     const char * theErrorLocation /* = (const char *)NULL */ )
{
#ifdef THREADED
  return ThreadManager::getGlobalThreadManager()
           . fullyUnlockMasterMutexReturningNumberOfLocks( theErrorLocation );
#else  /* THREADED */
  return 0;
#endif /* THREADED */
}

void TCM_FullyRelockMasterMutexNumberOfLocksTimes (
		     unsigned int theNumberOfLocks,
		     const char * theErrorLocation /* = (const char *)NULL */ )
{
#ifdef THREADED
  ThreadManager::getGlobalThreadManager()
    . fullyRelockMasterMutexNumberOfLocksTimes ( theNumberOfLocks,
						 theErrorLocation );
#endif /* THREADED */
}


int TCM_GetNumberOfThreads()
{
#ifdef THREADED
  TCM_LockMasterMutex("TCM_GetNumberOfThreads");
  int  returnValue
         = ThreadManager::getGlobalThreadManager().getNumberOfThreads();
  TCM_UnlockMasterMutex("TCM_GetNumberOfThreads");
  return returnValue;
#else  /* THREADED */
  return 0;
#endif /* THREADED */
}

TCM_Return_Type TCM_SetNumberOfThreads(unsigned int theNumberOfThreadsUnsigned)
{
  TCM_Return_Type returnValue = TCM_Ok;

#ifdef THREADED
  TCM_LockMasterMutex("TCM_SetNumberOfThreads");
  if ( ThreadManager::getGlobalThreadManager()
         . setNumberOfThreads ( theNumberOfThreadsUnsigned ) != TRUE )
    returnValue = TCM_Error;
  TCM_UnlockMasterMutex("TCM_SetNumberOfThreads");
#endif /* THREADED */

  return returnValue;
}


int TCM_GetMaximumNumberOfDynamiclyAllocatedThreads()
{
#ifdef THREADED
  TCM_LockMasterMutex("TCM_GetNumberOfThreads");
  int  returnValue = ThreadManager::getGlobalThreadManager()
		       . getMaximumNumberOfDynamiclyAllocatedThreads();
  TCM_UnlockMasterMutex("TCM_GetNumberOfThreads");
  return returnValue;
#else  /* THREADED */
  return 0;
#endif /* THREADED */
}

void TCM_SetMaximumNumberOfDynamiclyAllocatedThreads( int theMaximumNumber )
{
#ifdef THREADED
  TCM_LockMasterMutex("TCM_SetNumberOfThreads");
  ThreadManager::getGlobalThreadManager()
    . setMaximumNumberOfDynamiclyAllocatedThreads ( theMaximumNumber );
  TCM_UnlockMasterMutex("TCM_SetNumberOfThreads");
#endif /* THREADED */
}


void TCM_WaitForAllThreadsToFinishWork()
{
#ifdef THREADED
  ThreadManager::getGlobalThreadManager().waitForAllThreadsToFinishWork();
#else  /* THREADED */
  return;
#endif /* THREADED */
}


    /* This function is *ONLY* available when TCM has been compiled    *
     * with threading enabled.                                         *
     *                                                                 *
     * In this manner, attempting to use threads when TCM has not been *
     * compiled for threads will result in a link-time error.          */
#ifdef THREADED
TCM_Return_Type TCM_SetIsThreadedTask (
			    const TCM_Task_Tree_Ref & theTaskTreeRef,
			    BOOLEAN                   theIsThreaded /*=TRUE*/ )
{
  TCM_LockMasterMutex ( "TCM_SetIsThreadedTask" );
  RETURN_UNLOCK_IF_INVALID_NODE ( theTaskTreeRef, "TCM_SetIsThreadedTask" );

  theTaskTreeRef -> setIsThreaded ( theIsThreaded );

  TCM_UnlockMasterMutex ( "TCM_SetIsThreadedTask" );
  return TCM_Ok;
}
#endif /* THREADED */


BOOLEAN TCM_GetIsThreadedTask ( const TCM_Task_Tree_Ref & theTaskTreeRef )
{
#ifdef THREADED
  TCM_LockMasterMutex ( "TCM_GetIsThreadedTask" );
  RETURN_UNLOCK_ON_INVALID_NODE ( theTaskTreeRef, "TCM_SetIsThreadedTask",
				  FALSE );

  BOOLEAN  returnValue =  theTaskTreeRef -> getIsThreaded();

  TCM_UnlockMasterMutex ( "TCM_GetIsThreadedTask" );
  return returnValue;
#else  /* THREADED */
  return FALSE;
#endif /* THREADED */
}


   /* This function only exists if threading is used. (-DTHREADED) *
    * It is not needed in the non-threaded case, nor is it viable. */
#ifdef THREADED
static void BlockThreadUntilPointCallbackFunction ( const void * theData )
{
  Semaphore * aSemaphorePtr = (Semaphore *)theData;
  aSemaphorePtr -> wakeupOtherThread ( TRUE );
}
#endif /* THREADED */


   /* This function only exists if threading is used. (-DTHREADED) *
    * It is not needed in the non-threaded case, nor is it viable. */
#ifdef THREADED
void
TCM_BlockThreadUntilPoint ( const TCM_Point & theWaitTimePoint )
{
  Semaphore    aSemaphore;
  unsigned int lockCount;

  TCM_LockMasterMutex ( "BlockThreadUntilPoint" );


  TCM_InvokeWhen ( theWaitTimePoint,
		   BlockThreadUntilPointCallbackFunction,
		   (void *) (& aSemaphore) );


	/* If we have the lock, the semaphore can never be wakeup()'ed. */
  lockCount = TCM_FullyUnlockMasterMutexReturningNumberOfLocks (
						 "TCM_BlockThreadUntilPoint" );
	/* Block pending callback */
  aSemaphore . waitForSignal ( TRUE );

	/* If we don't restore those locks... */
  TCM_FullyRelockMasterMutexNumberOfLocksTimes ( lockCount,
						 "TCM_BlockThreadUntilPoint" );

  TCM_UnlockMasterMutex ( "BlockThreadUntilPoint" );
}
#endif /* THREADED */



   /* This function only exists if threading is used. (-DTHREADED) *
    * It is not needed in the non-threaded case, nor is it viable. */
#ifdef THREADED
void TCM_DebugGetPrimaryThreadId( void * theThreadId )
{
  TCM_LockMasterMutex ( "TCM_DebugGetPrimaryThreadId" );
  * ((THREAD_ID *) theThreadId) = GetPrimaryThreadId();
  TCM_UnlockMasterMutex ( "TCM_DebugGetPrimaryThreadId" );
}
#endif /* THREADED */




  /*
   * Thread-Task mapping retrieving/binding functions.
   */
TCM_Task_Tree_Ref TCM_GetCurrentTaskTreeRefForThisThread(
				BOOLEAN theCheckUserSpecifiedOnly /*= FALSE*/ )
{
  TCM_LockMasterMutex("TCM_GetCurrentTaskTreeRefForThisThread");

  TCM_Task_Tree_Ref currentTaskTreeRef
    = getCurrentTaskTreeRefForThisThread ( theCheckUserSpecifiedOnly );

  if (   ( currentTaskTreeRef . isNull()      )
      && ( theCheckUserSpecifiedOnly == FALSE ) )
    currentTaskTreeRef = TCM_RootNode();

  TCM_UnlockMasterMutex("TCM_GetCurrentTaskTreeRefForThisThread");

  return currentTaskTreeRef;
}

void TCM_PushUserTaskForThisThread ( const TCM_Task_Tree_Ref & theTaskTreeRef )
{
  TCM_LockMasterMutex("TCM_PushUserTaskForThisThread");  
  pushUserTaskForThisThread ( theTaskTreeRef );
  TCM_UnlockMasterMutex("TCM_PushUserTaskForThisThread");
}

TCM_Return_Type TCM_PopUserTaskForThisThread()
{
  TCM_LockMasterMutex("TCM_PopUserTaskForThisThread");  
  TCM_Return_Type returnValue = popUserTaskForThisThread();
  TCM_UnlockMasterMutex("TCM_PopUserTaskForThisThread");
  return returnValue;
}

void TCM_DebugPrintUserTaskForThreadStack( STRING theHeading,      /*= NULL*/
					   BOOLEAN theShowThreadId /*= TRUE*/ )
{
  TCM_LockMasterMutex("TCM_DebugPrintUserTaskForThreadStack");  
  debugPrintUserTaskForThreadStack( theHeading, theShowThreadId );
  TCM_UnlockMasterMutex("TCM_DebugPrintUserTaskForThreadStack");
}

