/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1999 Reid Simmons.  All rights reserved.
 *
 * FILE: distributed.cc
 *
 * ABSTRACT: Functions for creating distributed task tree nodes using
 *           message-passing protocols.
 *
 * $Revision: 1.23 $
 * $Date: 2014/02/28 02:24:40 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: distributed.cc,v $
 * Revision 1.23  2014/02/28 02:24:40  reids
 * Small update
 *
 * Revision 1.22  2009/05/04 19:44:49  reids
 * Changed to using snprintf to avoid corrupting the stack on overflow
 *
 * Revision 1.21  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.20  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.19  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.18  2003/04/17 21:08:55  da0g
 * Added code to free (cleanup) data associated with distributed invocation.
 * Added code to support [taskname] overloaded tasks.
 *
 * Revision 1.17  2002/09/16 22:47:12  da0g
 * Both Virtual and Actual distributed nodes have same instance name.
 *
 * Revision 1.16  2002/07/11 03:51:43  da0g
 * Addressed String = (char*) vs (const char *) issues.
 * Patched distributed instance name setting.
 * Addressed minor threading issues.
 * TCM_AllocateDistributedNode() now sets either/both
 *   the local-instance-name and the remote-instance-name.
 *
 * Revision 1.15  2002/06/26 16:48:52  reids
 * Made a distinction between the type-name and instance-name of a node.
 *  Enable instance name of node to be set in a distributed fashion.
 *  Removed a memory leak in distributedCallbackFn.
 *
 * Revision 1.14  2002/03/26 05:19:51  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.13  2002/03/22 02:24:08  da0g
 * TCM_* functions now properly lock/unlock the master mutex.
 * Removed TCM_IsDistributedNode()
 *
 * Revision 1.12  2002/02/05 17:45:16  reids
 * Backed out the getLocal function for distributed nodes -- instead,
 *   "getVirtual" creates a new virtual node only if the "host" given as the
 *   virtual address is not the current agent (o/w the node is "local").
 * Fixed several bugs relating to race conditions in the distributed version.
 *
 * Revision 1.11  2002/01/18 14:18:18  reids
 * Added "getLocal" to prevent race condition bugs with deleted virtual nodes
 *
 * Revision 1.10  2002/01/11 02:18:31  da0g
 * Fix: distributed.cc needed to keep copy of string, not reference.
 *
 * Revision 1.9  2001/08/07 23:50:06  da0g
 * Fixed TCM_ConnectDistributedAgent()'s invocation of
 *   agentConnectionIndex() to work.
 * Added TCM_IsDistributedNode().
 * Fixed:  Renamed removeRequestedEvents to removeRequestedEvent.
 *
 * Revision 1.8  2001/07/24 12:49:02  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.7  2001/03/26 21:38:55  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.6  2001/02/27 20:27:22  trey
 * added TCM_CreateLocalNode() call
 *
 * Revision 1.5  2001/02/27 02:37:02  trey
 * added client data to action and allocation callbacks; made communications go to the main IPC context by default when we don't explicitly connect
 *
 * Revision 1.4  2001/02/18 18:17:08  reids
 * Fixed a bug having to do with sending distributed task arguments
 *
 * Revision 1.3  2001/02/17 03:29:25  reids
 * Improved distributed version of TCM.  No longer does each agent have to
 *  connect to a separate server.  Also, simplified the communications
 *  interface since IPC now (version 3.4) supports automatic unmarshalling of
 *  data when invoking message handlers.
 *
 * Revision 1.2  1999/08/05 17:22:34  reids
 * Changes needed because the API to the commInterface library was updated
 *   to be more generally applicable.  Removed all dependencies (at least in
 *   theory) on IPC.
 *
 * Revision 1.1  1999/08/04 14:00:17  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 **************************************************************************/

#include "taskTree.h"
#include "tcmGlobal.h"
#include "virtual.h"
#include "distributed.h"
#include "commInterface.h"
#include "exception.h"
#if (__GNUC__ < 4) || (__GNUC__ == 4 && __GNUC_MINOR__ < 6) 
/*
 * gcc complains about not using <hash_map>, but that is not actually 
 * allowable under my current compiler (3.2.2).  This turns off the warnings.
 */
#define _CPP_BACKWARD_BACKWARD_WARNING_H
#define _BACKWARD_BACKWARD_WARNING_H
#include <hash_map.h>
#else
#endif

#define DEFAULT_STRING_BUFFER_SIZE 1023

static Named_Object thisAgentName;
static Agent_Connection_List agentConnections;
static Distributed_Task_List distributedTasks;

static hash_map<STRING, TCM_Exception::Creator, 
		hash<STRING>, eqstr> distributedExceptionCreators;

/**************************************************************************
 * 
 * Local Functions
 *
 **************************************************************************/

static BOOLEAN
verifyOverloadedTaskNameIndex(STRING theOverloadedTaskNameIndex)
{
  if ( theOverloadedTaskNameIndex == STRING(NULL) )
    return FALSE;

  for ( ;
	( * theOverloadedTaskNameIndex ) != NULL_CHAR;
	theOverloadedTaskNameIndex ++ )
  {
    if (    ( * theOverloadedTaskNameIndex )
	 == DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX )
      return FALSE;
  }

  return TRUE;
}

static Distributed_Task *distributedTaskRetrieve (
				STRING theTaskName,
				STRING theOverloadedTaskNameIndex
					 = DEFAULT_OVERLOADED_TASK_NAME_INDEX )
{
  Const_Distributed_Task_Iterator iter(distributedTasks);

  if ( theOverloadedTaskNameIndex == STRING(NULL) )
    theOverloadedTaskNameIndex = DEFAULT_OVERLOADED_TASK_NAME_INDEX;

  for ( ; iter; iter++) {
    if ( iter.getObject().equals( theTaskName, theOverloadedTaskNameIndex ) ) {
      return iter.getPointer();
    }
  }
  return (Distributed_Task *)NULL;
}

static STRING setActionMsgName (STRING theTaskName,
				STRING theOverloadedTaskNameIndex
					 = DEFAULT_OVERLOADED_TASK_NAME_INDEX )
{
  static size_t msgNameLength = DEFAULT_STRING_BUFFER_SIZE;
  static char * msgName       = new char [ DEFAULT_STRING_BUFFER_SIZE ];
  size_t        thisMessageNameLength;

  if ( theOverloadedTaskNameIndex == STRING(NULL) )
    theOverloadedTaskNameIndex = DEFAULT_OVERLOADED_TASK_NAME_INDEX;

  thisMessageNameLength = (  strlen(SET_ACTION_MSG_PREFIX)
			   + strlen(theOverloadedTaskNameIndex)
			   + 1 // DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX
			   + strlen(theTaskName)
			   + 1 // Ending NULL_CHAR
			   );

  if ( thisMessageNameLength > msgNameLength )
  {
    delete [] msgName;
    msgName       = new char [ thisMessageNameLength ];
    msgNameLength = thisMessageNameLength;
  }

  snprintf(msgName, DEFAULT_STRING_BUFFER_SIZE-1, "%s%s%c%s",
	   SET_ACTION_MSG_PREFIX, theOverloadedTaskNameIndex,
	   DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX,
	   theTaskName);
  return msgName;
}

   /* Copies OverloadedTaskNameIndex to theOverloadedTaskNameIndex. */
static void getOverloadedTaskNameIndexFromSetActionMsgName (
					  STRING theSetActionMsgName,
					  char * theOverloadedTaskNameIndex,
					  int    theMaxStringSize )
{
  int    i         = 0;
  size_t offset    = strlen(thisAgent()) + 1 + strlen(SET_ACTION_MSG_PREFIX);
  STRING tmpString = theSetActionMsgName + offset;

  if ( strlen(theSetActionMsgName) < offset )
  {
    tcmError( "getOverloadedTaskNameIndexFromSetActionMsgName: "
	      "Internal Error: theSetActionMsgName does not contain "
	      "OverloadedTaskNameIndex or TaskName.\n" );
    * theOverloadedTaskNameIndex = NULL_CHAR;
    return;
  }

  /* tmpString should now equal "%s%c%s",                    *
   * with %c = DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX */

  while (   ( (    ++ i   )  < theMaxStringSize                             )
	 && ( (* tmpString) != DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX)
	 && ( (* tmpString) != NULL_CHAR                                    ) )
  {
    * (theOverloadedTaskNameIndex ++) =  * (tmpString ++);
  }

  * theOverloadedTaskNameIndex = NULL_CHAR;

  if ( ( * tmpString ) != DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX )
  {
    if ( ( * tmpString ) == NULL_CHAR )
      tcmError( "getOverloadedTaskNameIndexFromSetActionMsgName: "
		"Internal Error: theSetActionMsgName does not contain '%c'.\n",
		DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX );
    else
      tcmError( "getOverloadedTaskNameIndexFromSetActionMsgName: "
		"Internal Error: theSetActionMsgName does not contain '%c' "
		"within the first %d characters.\n",
		DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX,
		theMaxStringSize );
  }
}


static STRING getTaskNameFromSetActionMsgName (STRING theSetActionMsgName)
{
  size_t offset    = strlen(thisAgent()) + 1 + strlen(SET_ACTION_MSG_PREFIX);
  STRING tmpString = theSetActionMsgName + offset;

  if ( strlen(theSetActionMsgName) < offset )
  {
    tcmError( "getTaskNameFromSetActionMsgName: "
	      "Internal Error: theSetActionMsgName does not contain "
	      "OverloadedTaskNameIndex or TaskName.\n" );
    return "";
  }

  /* tmpString should now equal "%s%c%s",                    *
   * with %c = DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX */

  while ( TRUE )
  {
    switch ( * tmpString )
    {
      case DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX:
	return tmpString + 1;

      case NULL_CHAR:
	tcmError( "getTaskNameFromSetActionMsgName: Internal Error: "
		  "theSetActionMsgName does not contain '%c'.\n",
		  DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX );
	return "";

      default:
	tmpString ++;
	break;
    }
  }
}

static STRING actionMsgFormat (STRING taskDataFormat)
{
  static char actionFormat[DEFAULT_STRING_BUFFER_SIZE];

  snprintf(actionFormat, DEFAULT_STRING_BUFFER_SIZE, "{%s, *%s}",
	   SET_ACTION_FORMAT_PREFIX,
	   (taskDataFormat ? taskDataFormat : "int"));
  return actionFormat;
}

static STRING exceptionMsgName (STRING exceptionName)
{
  static char msgName[DEFAULT_STRING_BUFFER_SIZE];

  snprintf(msgName, DEFAULT_STRING_BUFFER_SIZE, "%s%s",
	   EXCEPTION_MSG_PREFIX, exceptionName);
  return msgName;
}

static STRING agentMessage (STRING msgName, STRING agentName = thisAgent())
{
  static char agentMessageName[DEFAULT_STRING_BUFFER_SIZE];

  snprintf(agentMessageName, DEFAULT_STRING_BUFFER_SIZE, "%s_%s",
	   agentName, msgName);
  return agentMessageName;
}

inline int agentConnectionIndex (STRING agentName,
				 int    theDefaultReturnValue = 0)
{
  Const_Agent_Connection_Iterator listIter(agentConnections);

  for ( ; listIter; listIter++) {
    if (!strcmp(agentName, listIter()->agentName))
      return listIter()->index;
  }
  // i think returning -1 for not found will cause an error.
  //    perhaps sensible...
  // return -1;
  // but if we return 0 messages will go to the main context when
  //    we don't explicitly connect.  i like this better.

  /* Addendum (da0g):  Returning 0 breaks TCM_ConnectDistributedAgent().   *
   * Using a configurable default-return-value to circumvent this problem. */
  return theDefaultReturnValue;
}

/**************************************************************************
 * 
 * Message-Passing Functions
 *
 **************************************************************************/

static void allocateTaskHnd (MESSAGE_REFERENCE ref, void *data)
{
  ALLOCATE_MSG_PTR msgData = (ALLOCATE_MSG_PTR)data;
  Distributed_Task *distributedTask;

  distributedTask = distributedTaskRetrieve(msgData->taskName,
					    msgData->overloadedTaskNameIndex);
  if (!distributedTask) {
    tcmError("allocateTaskHnd: "
	     "%s[%s] is not registered as a distributed task\n",
	     msgData->taskName, msgData->overloadedTaskNameIndex);
  } else {
    TCM_Task_Tree_Ref node = (*distributedTask->allocationFn)
      (distributedTask->allocationFnClientData);
    node->setVirtualAddress(msgData->vaddress);
	/* Removed:
	 *    node->Named_Object::operator=(msgData->taskName);
	 * (It overrides the task-instance-name.)
	 */
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

static void distributedDataCleanupFunction ( const char * ref, void * data )
{
  CommInterface::freeData(ref, (void **)&data);
}

static void setActionHnd (MESSAGE_REFERENCE ref, void *data)
{
  SET_ACTION_MSG_PTR msgData = (SET_ACTION_MSG_PTR)data;
  Distributed_Task * distributedTask;
  STRING             taskName;
  char *             overloadedTaskNameIndex;
  char               stringBuffer[DEFAULT_STRING_BUFFER_SIZE];
  BOOLEAN            shouldCleanup = TRUE;

  size_t refNameSize = strlen(CommInterface::getRefName(ref));

	/* For long strings, use dynamic allocation to avoid overruns. */
  if ( refNameSize > DEFAULT_STRING_BUFFER_SIZE )
    overloadedTaskNameIndex = new char [ refNameSize ];
  else	/* For shorter strings, we can use the stack... */
    overloadedTaskNameIndex = stringBuffer;

  getOverloadedTaskNameIndexFromSetActionMsgName(
				      (STRING)CommInterface::getRefName(ref),
				      overloadedTaskNameIndex,
				      refNameSize );
  taskName
    = getTaskNameFromSetActionMsgName((STRING)CommInterface::getRefName(ref));

  distributedTask = distributedTaskRetrieve(taskName,overloadedTaskNameIndex);

  if (!distributedTask) {
    tcmError( "setActionHnd: %s[%s] is not registered as a distributed task\n",
	      taskName, overloadedTaskNameIndex );
  } else {
    TCM_Task_Tree_Ref node = Task_Tree_Node::getVirtual(msgData->vaddress);
    if (node != NULL)
    {
      TCM_SetAction(node, ((*distributedTask->actionFn)
			   (node, (void *)msgData->args,
			    distributedTask->actionFnClientData)));
      TCM_GetAction(node)
	-> setDistributedCleanupData ( strdup (CommInterface::getRefName(ref)),
				       data,
				       distributedDataCleanupFunction );
	/* Postpone cleaning up until action is finished! */
      shouldCleanup = FALSE;
    }
    else
    {
      tcmError( "setActionHnd: %s[%s] "
		"has no previously allocated virtual node.\n",
		taskName, overloadedTaskNameIndex );
    }
  }

	/* Free space we may have allocated up above. */
  if ( refNameSize > DEFAULT_STRING_BUFFER_SIZE )
    delete [] overloadedTaskNameIndex;

	/* If we succeeded, we will cleanup everything, arguments included,
	 * automatically when the action is free'd.  (Relying on the end-user
	 * to do this is somewhat less than reliable.)
	 * However, if we failed up above, we need to clean up now.
	 */
  if ( shouldCleanup == TRUE )
  {
   //Null out the arguments so they don't get freed (up to the task to do that)
   //msgData->args = NULL;
    CommInterface::freeData(ref, (void **)(void *)&msgData);
  }
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the child node's "host" telling it to add
 *              parentNode as its parent.
 *              Assumes childNode is a virtual node and parentNode already
 *              has a virtual address.
 *
 **************************************************************************/
void distributedAddParent (const Task_Tree_Ref &childNode,
			   const Task_Tree_Ref &parentNode, BOOLEAN canBeLast)
{
  ADD_PARENT_MSG_TYPE msgData;

  msgData.parentAddress = parentNode->getVirtualAddress();
  msgData.childAddress = childNode->getVirtualAddress();
  msgData.canBeLast = canBeLast;
  CommInterface::sendMessage(VNODE_HOST_INDEX(childNode),
			     VNODE_MSG_NAME(ADD_PARENT_MSG, childNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "add parent" message.
 *
 **************************************************************************/
static void addParentHnd (MESSAGE_REFERENCE ref, void *data)
{
  ADD_PARENT_MSG_PTR msgData = (ADD_PARENT_MSG_PTR)data;
  Task_Tree_Ref parentNode, childNode;

  childNode = Task_Tree_Node::getVirtual(msgData->childAddress);
  parentNode = Task_Tree_Node::getVirtual(msgData->parentAddress);
  if (childNode != NULL && parentNode != NULL) {
    childNode->addParent(parentNode, msgData->canBeLast);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the parent node's "host" telling it to add
 *              childNode as one of its children.
 *              Assumes parentNode is a virtual node and childNode already
 *              has a virtual address.
 *
 **************************************************************************/
void distributedAddChild (const Task_Tree_Ref &parentNode,
			  const Task_Tree_Ref &childNode)
{
  ADD_CHILD_MSG_TYPE msgData;

  msgData.parentAddress = parentNode->getVirtualAddress();
  msgData.childAddress = childNode->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(parentNode),
			     VNODE_MSG_NAME(ADD_CHILD_MSG, parentNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "add child" message.
 *
 **************************************************************************/
static void addChildHnd (MESSAGE_REFERENCE ref, void *data)
{
  ADD_CHILD_MSG_PTR msgData = (ADD_CHILD_MSG_PTR)data;
  Task_Tree_Ref parentNode, childNode;

  parentNode = Task_Tree_Node::getVirtual(msgData->parentAddress);
  childNode = Task_Tree_Node::getVirtual(msgData->childAddress);
  if (parentNode != NULL && childNode != NULL) {
    parentNode->addChild(childNode);
    if (parentNode->isSuspended()) {
      tcmMessage("Suspending child node %s from %s\n", parentNode->getName(),
		 childNode->getName());
      childNode->suspend();
    }
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the parent node's "host" telling it to 
 *              remove childNode from its list of children.
 *              Assumes parentNode is a virtual node and childNode already
 *              has a virtual address.
 *
 **************************************************************************/
void distributedRemoveChild (const Task_Tree_Ref &parentNode,
			     const Task_Tree_Ref &childNode)
{
  REMOVE_CHILD_MSG_TYPE msgData;

  msgData.parentAddress = parentNode->getVirtualAddress();
  msgData.childAddress = childNode->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(parentNode),
			     VNODE_MSG_NAME(REMOVE_CHILD_MSG, parentNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "remove child" message.
 *
 **************************************************************************/
static void removeChildHnd (MESSAGE_REFERENCE ref, void *data)
{
  REMOVE_CHILD_MSG_PTR msgData = (REMOVE_CHILD_MSG_PTR)data;
  Task_Tree_Ref parentNode, childNode;

  parentNode = Task_Tree_Node::getVirtual(msgData->parentAddress);
  childNode = Task_Tree_Node::getVirtual(msgData->childAddress);
  if (parentNode != NULL && childNode != NULL) {
    parentNode->removeChild(childNode);
    childNode->Task_Tree_Node::addParent(NULL, TRUE, FALSE);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the signalled node telling it to add
 *              <signal, signallingNode> as an expected event.
 *              Assumes signalledNode is a virtual node and signallingNode
 *              already has a virtual address.
 *
 **************************************************************************/
void distributedAddExpectedEvent (const Task_Tree_Ref &signalledNode,
				  Signal_Enum signal,
				  const Task_Tree_Ref &signallingNode,
				  const void *data)
{
  ADD_EXPECTED_MSG_TYPE msgData;

  msgData.signalledAddress = signalledNode->getVirtualAddress();
  msgData.signallingAddress = signallingNode->getVirtualAddress();
  msgData.signal = signal;
  msgData.timerData = (isTimerNode(signallingNode) ? thisAgent() : NULL);
  CommInterface::sendMessage(VNODE_HOST_INDEX(signalledNode),
			     VNODE_MSG_NAME(ADD_EXPECTED_MSG, signalledNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "add expected event" message.
 *
 **************************************************************************/
static void addExpectedHnd (MESSAGE_REFERENCE ref, void *data)
{
  ADD_EXPECTED_MSG_PTR msgData = (ADD_EXPECTED_MSG_PTR)data;
  Task_Tree_Ref signalledNode, signallingNode;

  signalledNode = Task_Tree_Node::getVirtual(msgData->signalledAddress);
  signallingNode = Task_Tree_Node::getVirtual(msgData->signallingAddress);
  if (signalledNode != NULL && signallingNode != NULL) {
    signalledNode->addExpectedEvent(msgData->signal, signallingNode,
				    msgData->timerData);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the signalled node signalling the event
 *              <signal, signallingNode>.
 *              Assumes signalledNode is a virtual node and signallingNode
 *              already has a virtual address.
 *
 **************************************************************************/
void distributedSignal (const Task_Tree_Ref &signalledNode, const Event &event)
{
  SIGNAL_MSG_TYPE msgData;

  msgData.signalledAddress = signalledNode->getVirtualAddress();
  msgData.signallingAddress = event.getNode()->getVirtualAddress();
  msgData.signal = event.getSignal();
  CommInterface::sendMessage(VNODE_HOST_INDEX(signalledNode),
			     VNODE_MSG_NAME(SIGNAL_MSG, signalledNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "signal" message.
 *
 **************************************************************************/
static void signalHnd (MESSAGE_REFERENCE ref, void *data)
{
  SIGNAL_MSG_PTR msgData = (SIGNAL_MSG_PTR)data;
  Task_Tree_Ref signalledNode, signallingNode;

  signalledNode = Task_Tree_Node::getVirtual(msgData->signalledAddress);
  signallingNode = Task_Tree_Node::getVirtual(msgData->signallingAddress);
  if (signalledNode != NULL && signallingNode != NULL) {
    Event event(msgData->signal, signallingNode);
    signalledNode->signal(event);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the signalling node telling it to add
 *              <state, signal, signalledNode> as a requested event.
 *              Assumes signallingNode is a virtual node and signalledNode
 *              already has a virtual address.
 *
 **************************************************************************/
void distributedAddRequestedEvent (const Task_Tree_Ref &signallingNode,
				   State_Enum state, Signal_Enum signal,
				   const Task_Tree_Ref &signalledNode)
{
  ADD_REQUESTED_MSG_TYPE msgData;

  msgData.signallingAddress = signallingNode->getVirtualAddress();
  msgData.signalledAddress = signalledNode->getVirtualAddress();
  msgData.state = state;
  msgData.signal = signal;
  CommInterface::sendMessage(VNODE_HOST_INDEX(signallingNode),
			     VNODE_MSG_NAME(ADD_REQUESTED_MSG, signallingNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "add requested event" message.
 *
 **************************************************************************/
static void addRequestedHnd (MESSAGE_REFERENCE ref, void *data)
{
  ADD_REQUESTED_MSG_PTR msgData = (ADD_REQUESTED_MSG_PTR)data;
  Task_Tree_Ref signalledNode, signallingNode;

  signallingNode = Task_Tree_Node::getVirtual(msgData->signallingAddress);
  signalledNode = Task_Tree_Node::getVirtual(msgData->signalledAddress);
  if (signallingNode != NULL && signalledNode != NULL) {
    signallingNode->addRequestedEvent(msgData->state, msgData->signal,
				      signalledNode);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the signalling node telling it to add
 *              a distributed callback event which will trigger when "state"
 *              holds.  Note that the callback function resides on a
 *              different process from the virtual node!
 *              Assumes signallingNode is a virtual node.
 *
 **************************************************************************/
void distributedAddRequestedCallback (const Task_Tree_Ref &signallingNode,
				      State_Enum state)
{
  ADD_REQUESTED_CALLBACK_MSG_TYPE msgData;

  msgData.signallingAddress = signallingNode->getVirtualAddress();
  msgData.state = state;
  msgData.host = thisAgent();
  CommInterface::sendMessage(VNODE_HOST_INDEX(signallingNode),
			     VNODE_MSG_NAME(ADD_REQUESTED_CALLBACK_MSG,
					    signallingNode),
			     &msgData);
}

static void distributedCallbackFn (const void *data)
{
  ADD_REQUESTED_CALLBACK_MSG_PTR callbackData;
  Task_Tree_Ref signallingNode;

  callbackData = (ADD_REQUESTED_CALLBACK_MSG_PTR)data;
  signallingNode = Task_Tree_Node::getVirtual(callbackData->signallingAddress);
  if (signallingNode != NULL) {
    distributedInvokeCallback(signallingNode, callbackData->state,
			      callbackData->host);
  }
  CommInterface::freeData(agentMessage(ADD_REQUESTED_CALLBACK_MSG),
			  (void **)(void *)&data);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "add requested callback" message.
 *
 **************************************************************************/
static void addRequestedCallbackHnd (MESSAGE_REFERENCE ref, void *data)
{
  ADD_REQUESTED_CALLBACK_MSG_PTR msgData =(ADD_REQUESTED_CALLBACK_MSG_PTR)data;
  Task_Tree_Ref signallingNode;

  signallingNode = Task_Tree_Node::getVirtual(msgData->signallingAddress);
  if (signallingNode != NULL) {
    signallingNode->addRequestedEvent(msgData->state, distributedCallbackFn,
				      msgData);
  }
  // Message data will be freed later, when callback is invoked
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the signalling node telling it to add
 *              a distributed callback event which will trigger when "state"
 *              holds.  Note that the callback function resides on a
 *              different process from the virtual node!
 *              Assumes signallingNode is a virtual node.
 *
 **************************************************************************/
void distributedInvokeCallback (const Task_Tree_Ref &signallingNode,
				State_Enum state, STRING host)
{
  INVOKE_CALLBACK_MSG_TYPE msgData;

  msgData.signallingAddress = signallingNode->getVirtualAddress();
  msgData.state = state;
  msgData.isTerminating = signallingNode->validState(Terminating_State);
  CommInterface::sendMessage(agentConnectionIndex(host),
			     agentMessage(INVOKE_CALLBACK_MSG, host), &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "invoke callback" message.
 *
 **************************************************************************/
static void invokeCallbackHnd (MESSAGE_REFERENCE ref, void *data)
{
  INVOKE_CALLBACK_MSG_PTR msgData = (INVOKE_CALLBACK_MSG_PTR)data;
  Task_Tree_Ref signallingNode;

  signallingNode = Task_Tree_Node::getVirtual(msgData->signallingAddress);
  ((Virtual_Node *)*signallingNode)->setIsTerminating(msgData->isTerminating);
  ((Virtual_Node *)*signallingNode)->invokeCallback(msgData->state);
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}


/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the signalling node telling it to remove
 *              the expected event that matches the given signal and
 *              signallingNode.
 *              Assumes signalledNode is a virtual node and signallingNode
 *              already has a virtual address.
 *
 **************************************************************************/
void distributedRemoveExpectedEvent (const Task_Tree_Ref &signalledNode,
				     Signal_Enum signal,
				     const Task_Tree_Ref &signallingNode)
{
  REMOVE_EXPECTED_MSG_TYPE msgData;

  msgData.signalledAddress = signalledNode->getVirtualAddress();
  msgData.signallingAddress = signallingNode->getVirtualAddress();
  msgData.signal = signal;
  CommInterface::sendMessage(VNODE_HOST_INDEX(signalledNode),
			     VNODE_MSG_NAME(REMOVE_EXPECTED_MSG, signalledNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "remove expected events" message.
 *
 **************************************************************************/
static void removeExpectedHnd (MESSAGE_REFERENCE ref, void *data)
{
  REMOVE_EXPECTED_MSG_PTR msgData = (REMOVE_EXPECTED_MSG_PTR)data;
  Task_Tree_Ref signalledNode, signallingNode;

  signalledNode = Task_Tree_Node::getVirtual(msgData->signalledAddress);
  signallingNode = Task_Tree_Node::getVirtual(msgData->signallingAddress);
  if (signalledNode != NULL && signallingNode != NULL) {
    signalledNode->removeExpectedEvent(msgData->signal, signallingNode);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the signalling node telling it to remove
 *              all the <signal, signalledNode, state> requested events
 *              that match the given signal and signalledNode.
 *              Assumes signallingNode is a virtual node and signalledNode
 *              already has a virtual address.
 *
 **************************************************************************/
void distributedRemoveRequestedEvents (const Task_Tree_Ref &signallingNode,
				       Signal_Enum signal,
				       const Task_Tree_Ref &signalledNode)
{
  REMOVE_REQUESTED_MSG_TYPE msgData;

  msgData.signallingAddress = signallingNode->getVirtualAddress();
  msgData.signalledAddress = signalledNode->getVirtualAddress();
  msgData.signal = signal;
  CommInterface::sendMessage(VNODE_HOST_INDEX(signallingNode),
			     VNODE_MSG_NAME(REMOVE_REQUESTED_MSG,
					    signallingNode),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "remove requested events" message.
 *
 **************************************************************************/
static void removeRequestedHnd (MESSAGE_REFERENCE ref, void *data)
{
  REMOVE_REQUESTED_MSG_PTR msgData = (REMOVE_REQUESTED_MSG_PTR)data;
  Task_Tree_Ref signalledNode, signallingNode;

  signallingNode = Task_Tree_Node::getVirtual(msgData->signallingAddress);
  signalledNode = Task_Tree_Node::getVirtual(msgData->signalledAddress);
  if (signallingNode != NULL && signalledNode != NULL) {
    Event event(msgData->signal, signallingNode);
    signalledNode->removeRequestedEvent(event);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Remove the timed event from the given host.
 *              Assumes that the node has a virtual address.
 *
 **************************************************************************/

void Virtual_Node::distributedRemoveTimer (Signal_Enum signal,
					   STRING timerHost) const
{
  REMOVE_TIMER_MSG_TYPE msgData;

  msgData.vaddress = getVirtualAddress();
  msgData.signal = signal;
  CommInterface::sendMessage(agentConnectionIndex(timerHost),
			     agentMessage(REMOVE_TIMER_MSG, timerHost),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "remove timer events" message.
 *
 **************************************************************************/
static void removeTimerHnd (MESSAGE_REFERENCE ref, void *data)
{
  REMOVE_TIMER_MSG_PTR msgData = (REMOVE_TIMER_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);

  Timed_Event timedEvent(msgData->signal, nodeRef, 0);
  GET_TCM_GLOBAL(agenda).removeTimer(timedEvent);
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node that it should be considered to
 *              have completed handing.  Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedDoneHandling (const Task_Tree_Ref &nodeRef)
{
  DONE_HANDLING_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(DONE_HANDLING_MSG, nodeRef),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "doneHandling" message.
 *
 **************************************************************************/
static void doneHandlingHnd (MESSAGE_REFERENCE ref, void *data)
{
  DONE_HANDLING_MSG_PTR msgData = (DONE_HANDLING_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->doneHandling();
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it to terminate itself
 *              and its children.  Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedTerminate (const Task_Tree_Ref &nodeRef)
{
  TERMINATE_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(TERMINATE_MSG, nodeRef), &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "terminate" message.
 *
 **************************************************************************/
static void terminateHnd (MESSAGE_REFERENCE ref, void *data)
{
  TERMINATE_MSG_PTR msgData = (TERMINATE_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->terminate();
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it to deallocate itself.
 *              Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedDeallocate (const Task_Tree_Ref &nodeRef)
{
  DEALLOCATE_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(DEALLOCATE_MSG, nodeRef), &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "deallocate" message.
 *
 **************************************************************************/
static void deallocateHnd (MESSAGE_REFERENCE ref, void *data)
{
  DEALLOCATE_MSG_PTR msgData = (DEALLOCATE_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->deallocate();
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it to suspend itself
 *              and its children.  Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedSuspend (const Task_Tree_Ref &nodeRef)
{
  SUSPEND_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(SUSPEND_MSG, nodeRef), &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "suspend" message.
 *
 **************************************************************************/
static void suspendHnd (MESSAGE_REFERENCE ref, void *data)
{
  SUSPEND_MSG_PTR msgData = (SUSPEND_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->suspend();
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it to unsuspend itself
 *              and its children.  Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedUnsuspend (const Task_Tree_Ref &nodeRef)
{
  UNSUSPEND_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(UNSUSPEND_MSG, nodeRef), &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "unsuspend" message.
 *
 **************************************************************************/
static void unsuspendHnd (MESSAGE_REFERENCE ref, void *data)
{
  UNSUSPEND_MSG_PTR msgData = (UNSUSPEND_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->unsuspend();
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it to delay termination.
 *              Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedDelayTermination (const Task_Tree_Ref &nodeRef)
{
  DELAY_TERMINATION_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(DELAY_TERMINATION_MSG, nodeRef),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "delayTermination" message.
 *
 **************************************************************************/
static void delayTerminationHnd (MESSAGE_REFERENCE ref, void *data)
{
  DELAY_TERMINATION_MSG_PTR msgData = (DELAY_TERMINATION_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->delayTermination();
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it to enable termination.
 *              Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedEnableTermination (const Task_Tree_Ref &nodeRef)
{
  ENABLE_TERMINATION_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(ENABLE_TERMINATION_MSG, nodeRef),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "enableTermination" message.
 *
 **************************************************************************/
static void enableTerminationHnd (MESSAGE_REFERENCE ref, void *data)
{
  ENABLE_TERMINATION_MSG_PTR msgData = (ENABLE_TERMINATION_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->enableTermination();
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node asking it to display its subtree.
 *              Assumes nodeRef is a virtual node.
 *
 * NOTES: Can only display to either stdout or stderr (whereas the regular
 *        version can display to any arbitrary stream/file).
 *
 **************************************************************************/
void distributedDisplayTree (const Task_Tree_Ref &nodeRef,
			     int whichStream, int position)
{
  DISPLAY_TREE_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  msgData.whichStream = whichStream;
  msgData.position = position;
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(DISPLAY_TREE_MSG, nodeRef),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "display tree" message.
 *
 **************************************************************************/
static void displayTreeHnd (MESSAGE_REFERENCE ref, void *data)
{
  DISPLAY_TREE_MSG_PTR msgData = (DISPLAY_TREE_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
    nodeRef->displayTree(msgData->whichStream == USE_STDOUT ? stdout : stderr,
			 msgData->position);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}


/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node's "host" telling it to add
 *              terminationRef as a new termination action
 *              Assumes nodeRef is a virtual node and terminationRef
 *              already has a virtual address.
 *
 **************************************************************************/
void distributedAddTermination (const Task_Tree_Ref &nodeRef,
				const Task_Tree_Ref &terminationNode)
{
  ADD_TERMINATION_MSG_TYPE msgData;

  msgData.vaddress = nodeRef->getVirtualAddress();
  msgData.terminationAddress = terminationNode->getVirtualAddress();
  msgData.terminationName = TCM_NodeName(terminationNode);
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(ADD_TERMINATION_MSG, nodeRef),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "add termination action" message.
 *
 **************************************************************************/
static void addTerminationHnd (MESSAGE_REFERENCE ref, void *data)
{
  ADD_TERMINATION_MSG_PTR msgData = (ADD_TERMINATION_MSG_PTR)data;
  Task_Tree_Ref nodeRef, terminationNode;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  terminationNode = Task_Tree_Node::getVirtual(msgData->terminationAddress,
					       msgData->terminationName);
  if (nodeRef != NULL && terminationNode != NULL) {
    nodeRef->addTerminationAction(terminationNode);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it to change the
 *              instance name of the node. Assumes nodeRef is a virtual node.
 *
 **************************************************************************/

void distributedSetInstanceName(const Task_Tree_Ref &nodeRef,
				STRING newInstanceName)
{
  SET_INSTANCE_NAME_MSG_TYPE msgData;

  msgData.vaddress     = nodeRef->getVirtualAddress();
  msgData.instanceName = newInstanceName;
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(SET_INSTANCE_NAME_MSG, nodeRef),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "set instance name action" message.
 *
 **************************************************************************/
static void setInstanceNameHnd (MESSAGE_REFERENCE ref, void *data)
{
  SET_INSTANCE_NAME_MSG_PTR msgData = (SET_INSTANCE_NAME_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->vaddress);
  if (nodeRef != NULL) {
	/* Don't do nodeRef->setInstanceName() here: Forms endless loop, *
	 * or crashes if Virtual_Address is not yet configured right.    */
    nodeRef->Task_Tree_Node::setInstanceName(msgData->instanceName);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node asking it find an exception
 *              handler for the failure and add an exception node.
 *              Assumes nodeRef is a virtual node and parentNode and
 *              failureNode already have virtual addresses.
 *
 **************************************************************************/

void distributedFindAndApplyExceptionHandler (const Task_Tree_Ref &nodeRef,
					      const Task_Tree_Ref &parentNode,
					      const Task_Tree_Ref &failureNode,
					      Exception_Instance *exception)
{
  FIND_EXCEPTION_HANDLER_MSG_TYPE msgData;

  msgData.contextNode = nodeRef->getVirtualAddress();
  msgData.parentNode =  parentNode->getVirtualAddress();
  msgData.failureNode = failureNode->getVirtualAddress();
  msgData.failure = exception->getName();

  bzero(&msgData.exceptionData, sizeof(msgData.exceptionData));
  if (exception->getExceptionData() != NULL) {
    unsigned length;
    if (CommInterface::marshallData(exceptionMsgName(msgData.failure),
				    (void *)exception->getExceptionData(),
				    &msgData.exceptionData.data,
				    &length) == SUCCESS) {
      msgData.exceptionData.length = length;
    } else {
      tcmError("Need to call TCM_RegisterDistributedException for '%s'\n",
	       msgData.failure);
    }
  }

  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(FIND_EXCEPTION_HANDLER_MSG,
					    nodeRef),
			     &msgData);
}

static TCM_Exception *createDistributedException (STRING name, const void *data)
{
  TCM_Exception::Creator creator = distributedExceptionCreators[name];

  if (creator == NULL) {
    tcmError("Exception '%s' not declared distributed on agent '%s'\n",
	     name, thisAgent());
    creator = create_TCM_Exception;
  }
  return (*creator)(name, data);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "find and apply exception handler" message.
 *
 **************************************************************************/
static void findAndApplyExceptionHandlerHnd (MESSAGE_REFERENCE ref, void *data)
{
  FIND_EXCEPTION_HANDLER_MSG_PTR msgData =(FIND_EXCEPTION_HANDLER_MSG_PTR)data;
  Task_Tree_Ref contextNode, parentNode, failureNode;

  contextNode = Task_Tree_Node::getVirtual(msgData->contextNode);
  parentNode  = Task_Tree_Node::getVirtual(msgData->parentNode);
  failureNode = Task_Tree_Node::getVirtual(msgData->failureNode);
  if (contextNode != NULL && parentNode != NULL && failureNode != NULL) {
    void *dataPtr = NULL;
    if (msgData->exceptionData.length > 0) {
      if (CommInterface::unmarshallData(exceptionMsgName(msgData->failure),
					msgData->exceptionData.data,
					&dataPtr) == SUCCESS) {
      } else {
	tcmError("Need to call TCM_RegisterDistributedException for '%s'\n",
		 msgData->failure);
      }
    }

    Exception_Instance exception(createDistributedException(msgData->failure,
							    dataPtr),
				 contextNode);
    contextNode->findAndApplyExceptionHandler(parentNode, failureNode,
					      &exception);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the node telling it that no exception 
 *              handler was found for the failure.
 *              Assumes nodeRef is a virtual node.
 *
 **************************************************************************/
void distributedNoExceptionHandler (const Task_Tree_Ref &nodeRef,
				    STRING failure)
{
  FIND_EXCEPTION_HANDLER_MSG_TYPE msgData;

  msgData.failureNode = nodeRef->getVirtualAddress();
  msgData.failure = failure;
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(FIND_EXCEPTION_HANDLER_MSG,
					    nodeRef),
			     &msgData);
}

/**************************************************************************
 * 
 * DESCRIPTION: Handler for the "no exception handler" message.
 *
 **************************************************************************/
static void noExceptionHandlerHnd (MESSAGE_REFERENCE ref, void *data)
{
  FIND_EXCEPTION_HANDLER_MSG_PTR msgData =(FIND_EXCEPTION_HANDLER_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->failureNode);
  if (nodeRef != NULL) {
    nodeRef->noExceptionHandler(msgData->failure);
  }
  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

void distributedSetPersistence (const Task_Tree_Ref nodeRef, BOOLEAN value)
{
  SET_PERSISTENCE_MSG_TYPE msgData;

  msgData.nodeRef = nodeRef->getVirtualAddress();
  msgData.value   = value;
  CommInterface::sendMessage(VNODE_HOST_INDEX(nodeRef),
			     VNODE_MSG_NAME(SET_PERSISTENCE_MSG, nodeRef),
			     &msgData);
}

static void setPersistenceHnd (MESSAGE_REFERENCE ref, void *data)
{
  SET_PERSISTENCE_MSG_PTR msgData = (SET_PERSISTENCE_MSG_PTR)data;
  Task_Tree_Ref nodeRef;

  nodeRef = Task_Tree_Node::getVirtual(msgData->nodeRef);
  if (nodeRef != NULL) {
    nodeRef->setPersistence(msgData->value);
  }

  CommInterface::freeData(ref, (void **)(void *)&msgData);
}

static void registerDistributedMessages (void)
{
  CommInterface::registerMessage(agentMessage(ALLOCATE_MSG),
				 ALLOCATE_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ALLOCATE_MSG), allocateTaskHnd);

  CommInterface::registerMessage(agentMessage(ADD_PARENT_MSG),
				 ADD_PARENT_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ADD_PARENT_MSG), addParentHnd);

  CommInterface::registerMessage(agentMessage(ADD_CHILD_MSG),
				 ADD_CHILD_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ADD_CHILD_MSG), addChildHnd);

  CommInterface::registerMessage(agentMessage(REMOVE_CHILD_MSG),
				 REMOVE_CHILD_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(REMOVE_CHILD_MSG),
				  removeChildHnd);

  CommInterface::registerMessage(agentMessage(SIGNAL_MSG), SIGNAL_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(SIGNAL_MSG), signalHnd);

  CommInterface::registerMessage(agentMessage(ADD_EXPECTED_MSG),
				 ADD_EXPECTED_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ADD_EXPECTED_MSG),
				  addExpectedHnd);

  CommInterface::registerMessage(agentMessage(ADD_REQUESTED_MSG),
				 ADD_REQUESTED_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ADD_REQUESTED_MSG),
				  addRequestedHnd);

  CommInterface::registerMessage(agentMessage(ADD_REQUESTED_CALLBACK_MSG),
				 ADD_REQUESTED_CALLBACK_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ADD_REQUESTED_CALLBACK_MSG),
				  addRequestedCallbackHnd);

  CommInterface::registerMessage(agentMessage(INVOKE_CALLBACK_MSG),
				 INVOKE_CALLBACK_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(INVOKE_CALLBACK_MSG),
				  invokeCallbackHnd);

  CommInterface::registerMessage(agentMessage(REMOVE_EXPECTED_MSG),
				 REMOVE_EXPECTED_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(REMOVE_EXPECTED_MSG),
				  removeExpectedHnd);

  CommInterface::registerMessage(agentMessage(REMOVE_REQUESTED_MSG),
				 REMOVE_REQUESTED_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(REMOVE_REQUESTED_MSG),
				  removeRequestedHnd);

  CommInterface::registerMessage(agentMessage(REMOVE_TIMER_MSG),
				 REMOVE_TIMER_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(REMOVE_TIMER_MSG),
				  removeTimerHnd);

  CommInterface::registerMessage(agentMessage(DONE_HANDLING_MSG),
				 DONE_HANDLING_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(DONE_HANDLING_MSG),
				  doneHandlingHnd);

  CommInterface::registerMessage(agentMessage(TERMINATE_MSG),
				 TERMINATE_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(TERMINATE_MSG), terminateHnd);

  CommInterface::registerMessage(agentMessage(DEALLOCATE_MSG),
				 DEALLOCATE_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(DEALLOCATE_MSG), deallocateHnd);

  CommInterface::registerMessage(agentMessage(SUSPEND_MSG), SUSPEND_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(SUSPEND_MSG), suspendHnd);

  CommInterface::registerMessage(agentMessage(UNSUSPEND_MSG),
				 UNSUSPEND_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(UNSUSPEND_MSG), unsuspendHnd);

  CommInterface::registerMessage(agentMessage(DELAY_TERMINATION_MSG),
				 DELAY_TERMINATION_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(DELAY_TERMINATION_MSG),
				  delayTerminationHnd);

  CommInterface::registerMessage(agentMessage(ENABLE_TERMINATION_MSG),
				 ENABLE_TERMINATION_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ENABLE_TERMINATION_MSG), 
				  enableTerminationHnd);

  CommInterface::registerMessage(agentMessage(ADD_TERMINATION_MSG),
				 ADD_TERMINATION_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(ADD_TERMINATION_MSG),
				  addTerminationHnd);

  CommInterface::registerMessage(agentMessage(SET_INSTANCE_NAME_MSG),
				 SET_INSTANCE_NAME_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(SET_INSTANCE_NAME_MSG),
				  setInstanceNameHnd);

  CommInterface::registerMessage(agentMessage(DISPLAY_TREE_MSG),
				 DISPLAY_TREE_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(DISPLAY_TREE_MSG),
				  displayTreeHnd);

  CommInterface::registerMessage(agentMessage(FIND_EXCEPTION_HANDLER_MSG),
				 FIND_EXCEPTION_HANDLER_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(FIND_EXCEPTION_HANDLER_MSG), 
				  findAndApplyExceptionHandlerHnd);

  CommInterface::registerMessage(agentMessage(NO_EXCEPTION_HANDLER_MSG),
				 NO_EXCEPTION_HANDLER_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(NO_EXCEPTION_HANDLER_MSG), 
				  noExceptionHandlerHnd);

  CommInterface::registerMessage(agentMessage(SET_PERSISTENCE_MSG),
				 SET_PERSISTENCE_MSG_FORMAT);
  CommInterface::subscribeMessage(agentMessage(SET_PERSISTENCE_MSG),
				  setPersistenceHnd);
}

/**************************************************************************
 * 
 * User-Interface Functions
 *
 **************************************************************************/

/**************************************************************************
 * 
 * DESCRIPTION: Return the name of the agent associated with this process.
 *
 **************************************************************************/
STRING thisAgent (void)
{
  return thisAgentName.getName();
}

/**************************************************************************
 * 
 * DESCRIPTION: Create and return a virtual node, and send a message to
 *              the "agentName" to create a node of "taskName".
 *
 **************************************************************************/
TCM_Task_Tree_Ref TCM_AllocateDistributedNode (
			     STRING theAgentName,
			     STRING theNodeTypeName,
			     STRING theInstanceName, /* = STRING(NULL) */
			     STRING theOverloadedTaskNameIndex
			             /*= DEFAULT_OVERLOADED_TASK_NAME_INDEX*/ )
{
  TCM_LockMasterMutex ( "TCM_AllocteaDistributedNode" );

  Task_Tree_Ref virtualNode;
  ALLOCATE_MSG_TYPE msgData;

  if ( theOverloadedTaskNameIndex == STRING(NULL) )
    theOverloadedTaskNameIndex = DEFAULT_OVERLOADED_TASK_NAME_INDEX;

  if ( verifyOverloadedTaskNameIndex(theOverloadedTaskNameIndex) == FALSE ) {
    tcmError("TCM_AllocateDistributedNode:  Error: "
	     "Delimiter ('%c') found in theOverloadedTaskNameIndex\n",
	     DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX);
  } else if (!CommInterface::getCommHasBeenInitialized()) {
    tcmError("Need to call TCM_EnableDistributedComm first\n");
  } else if (streq(theAgentName, thisAgent())) {
    tcmError("Trying to allocate a distributed node (%s) internally\n");
  } else {
    virtualNode = new Virtual_Node(theNodeTypeName, theAgentName, thisAgent());
	/* Cache value locally for optional use in TCM_SetDistributedAction */
    virtualNode->setOverloadedNodeTypeNameIndex( theOverloadedTaskNameIndex );
    msgData.vaddress                = virtualNode->getVirtualAddress();
    msgData.taskName                = theNodeTypeName;
    msgData.overloadedTaskNameIndex = theOverloadedTaskNameIndex;
    CommInterface::sendMessage( agentConnectionIndex(theAgentName),
			        agentMessage(ALLOCATE_MSG, theAgentName),
				& msgData);
    
    if ( theInstanceName != STRING(NULL) )
      TCM_SetInstanceName ( virtualNode, theInstanceName );
  }

  TCM_UnlockMasterMutex ( "TCM_AllocateDistributedNode" );
  return virtualNode;
}


/**************************************************************************
 * 
 * DESCRIPTION: Send a message to the virtual address of the node to
 *              set the action with the given data.
 *
 **************************************************************************/
TCM_Return_Type TCM_SetDistributedAction (
    const TCM_Task_Tree_Ref & nodeRef, 
    const void *              args,
    STRING                    theOverloadedTaskNameIndex
			       /*= USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE*/ )
{
  TCM_LockMasterMutex ( "TCM_SetDistributedAction" );

  SET_ACTION_MSG_TYPE msgData;
  STRING agentName;

  if (!IS_DISTRIBUTED(nodeRef)) {
    tcmError("TCM_SetDistributedAction: %s is not a distributed node\n",
	     nodeRef->instanceName());
    TCM_UnlockMasterMutex ( "TCM_SetDistributedAction" );
    return TCM_Error;
  } else if (!CommInterface::getCommHasBeenInitialized()) {
    tcmError("Need to call TCM_EnableDistributedComm first\n");
    TCM_UnlockMasterMutex ( "TCM_SetDistributedAction" );
    return TCM_Error;
  } else {
    if (theOverloadedTaskNameIndex == USE_VALUE_FROM_ALLOCATE_DISTRIBUTED_NODE)
      theOverloadedTaskNameIndex = nodeRef -> getOverloadedNodeTypeNameIndex();

    if ( verifyOverloadedTaskNameIndex(theOverloadedTaskNameIndex) == FALSE )
    {
      tcmError("TCM_SetDistributedAction:  Error: "
	       "Delimiter ('%c') found in theOverloadedTaskNameIndex\n",
	       DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX);
      TCM_UnlockMasterMutex ( "TCM_SetDistributedAction" );
      return TCM_Error;
    }

    msgData.vaddress = nodeRef->getVirtualAddress();
    msgData.args = args;
    agentName = nodeRef->getVirtualAddress().host;
    CommInterface::sendMessage(agentConnectionIndex(agentName),
			       agentMessage(
				 setActionMsgName(nodeRef->nodeTypeName(),
						  theOverloadedTaskNameIndex),
				 agentName),
			       & msgData);
    TCM_UnlockMasterMutex ( "TCM_SetDistributedAction" );
    return TCM_Ok;
  }
}

/**************************************************************************
 * 
 * DESCRIPTION: This is a shorthand for a series of allocate/setAction/insert
 *              calls
 *
 **************************************************************************/
TCM_Task_Tree_Ref TCM_CreateDistributedNode(
	 STRING                    agentName,
	 const TCM_Task_Tree_Ref & parentRef,
	 STRING                    taskName,
	 void *                    args,
	 BOOLEAN                   isLastChild, /* = TRUE */
	 STRING                    theOverloadedTaskNameIndex
				    /*= DEFAULT_OVERLOADED_TASK_NAME_INDEX*/ )
{
  TCM_LockMasterMutex ( "TCM_CreateDistributedNode" );

  TCM_Task_Tree_Ref nodeRef;
  if (!validRef(parentRef)) {
    tcmWarning("TCM_CreateDistributedNode: Reference to NULL node\n");
  } else if (!CommInterface::getCommHasBeenInitialized()) {
    tcmError("Need to call TCM_EnableDistributedComm first\n");
  } else if (streq(agentName, thisAgent())) {
    tcmError("Trying to allocate a distributed node (%s) internally\n");
  } else {
    nodeRef = TCM_AllocateDistributedNode(agentName,
					  taskName,
					  STRING(NULL),
					  theOverloadedTaskNameIndex);
    TCM_SetDistributedAction(nodeRef, args, theOverloadedTaskNameIndex);
    taskTreeInsert(*parentRef, *nodeRef, isLastChild);
  }

  TCM_UnlockMasterMutex ( "TCM_CreateDistributedNode" );
  return nodeRef;
}

/**************************************************************************
 * 
 * DESCRIPTION: This is a shorthand for a series of allocate/setAction/insert
 *              calls.  TCM_CreateLocalNode() is a convenient way to locally
 *              invoke a task which is registered as a distributed task,
 *              without going through the distributed task overhead.
 *
 **************************************************************************/
TCM_Task_Tree_Ref TCM_CreateLocalNode(
	 const TCM_Task_Tree_Ref & parentRef,
	 STRING                    taskName,
	 void *                    args,
	 BOOLEAN                   isLastChild, /* = TRUE */
	 STRING                    theOverloadedTaskNameIndex
				    /*= DEFAULT_OVERLOADED_TASK_NAME_INDEX*/ )
{
  TCM_LockMasterMutex ( "TCM_CreateLocalNode" );

  if ( theOverloadedTaskNameIndex == STRING(NULL) )
    theOverloadedTaskNameIndex = DEFAULT_OVERLOADED_TASK_NAME_INDEX;

  if ( verifyOverloadedTaskNameIndex(theOverloadedTaskNameIndex) == FALSE )
    tcmError("TCM_CreateLocalNode:  Warning: "
	     "Delimiter ('%c') found in theOverloadedTaskNameIndex\n",
	     DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX);

  Distributed_Task * distributedTask
		       = distributedTaskRetrieve( taskName,
						  theOverloadedTaskNameIndex );

  if (!distributedTask) {
    tcmError("TCM_CreateLocalNode: "
	     "%s[%s] is not registered as a distributed task\n",
	     taskName, theOverloadedTaskNameIndex );
  } else {
    TCM_Task_Tree_Ref node = (*distributedTask->allocationFn)
      (distributedTask->allocationFnClientData);
    TCM_SetAction(node, (*distributedTask->actionFn)
		  (node, args,
		   distributedTask->actionFnClientData));
    taskTreeInsert(*parentRef, *node, isLastChild);

    TCM_UnlockMasterMutex ( "TCM_CreateLocalNode" );
    return node;
  }

  TCM_UnlockMasterMutex ( "TCM_CreateLocalNode" );
  return 0;
}

/**************************************************************************
 * 
 * DESCRIPTION: Register "taskName" as a task that can be invoked by
 *              another module (using "CreateDistributedNode").
 *
 **************************************************************************/
TCM_Return_Type TCM_RegisterDistributedTask (
		STRING             taskName, 
		TASK_ALLOCATION_FN allocationFn,
		void *             allocationFnClientData,
		ACTION_CREATION_FN actionFn,
		void *             actionFnClientData,
		STRING             taskDataFormat,
		STRING             theOverloadedTaskNameIndex
			             /*= DEFAULT_OVERLOADED_TASK_NAME_INDEX*/ )
{
  TCM_LockMasterMutex ( "TCM_RegisterDistributedTask" );

  Distributed_Task distributedTask, *existingTask;
  STRING actionMsgName;

  if ( theOverloadedTaskNameIndex == STRING(NULL) )
    theOverloadedTaskNameIndex = DEFAULT_OVERLOADED_TASK_NAME_INDEX;

  if ( verifyOverloadedTaskNameIndex(theOverloadedTaskNameIndex) == FALSE )
  {
    tcmError("TCM_RegisterDistributedTask:  Error: "
	     "Delimiter ('%c') found in theOverloadedTaskNameIndex\n",
	     DELIMITER_CHAR_FOR_OVERLOADED_TASK_NAME_INDEX);
    TCM_UnlockMasterMutex ( "TCM_RegisterDistributedTask" );
    return TCM_Error;
  }

  if (!CommInterface::getCommHasBeenInitialized()) {
    tcmError("Need to call TCM_EnableDistributedComm first\n");
    TCM_UnlockMasterMutex ( "TCM_RegisterDistributedTask" );
    return TCM_Error;
  } else {
    existingTask = distributedTaskRetrieve(taskName,
					   theOverloadedTaskNameIndex);
    if (existingTask) {
      tcmWarning("%s is already registered as a distributed task\n", taskName);
      existingTask->allocationFn = allocationFn;
      existingTask->actionFn = actionFn;
      existingTask->allocationFnClientData = allocationFnClientData;
      existingTask->actionFnClientData = actionFnClientData;
    } else {
      distributedTask.taskName = taskName;
      distributedTask.overloadedTaskNameIndex = theOverloadedTaskNameIndex;
      distributedTask.allocationFn = allocationFn;
      distributedTask.actionFn = actionFn;
      distributedTask.allocationFnClientData = allocationFnClientData;
      distributedTask.actionFnClientData = actionFnClientData;
      distributedTasks.insertLast(distributedTask);
    }
    actionMsgName = agentMessage(setActionMsgName(taskName,
						  theOverloadedTaskNameIndex));
    CommInterface::registerMessage(actionMsgName,
				   actionMsgFormat(taskDataFormat));
    CommInterface::subscribeMessage(actionMsgName, setActionHnd);

    TCM_UnlockMasterMutex ( "TCM_RegisterDistributedTask" );
    return TCM_Ok;
  }
}

TCM_Return_Type TCM_EnableDistributedComm (STRING myName, STRING myHostMachine)
{
  TCM_LockMasterMutex ( "TCM_EnableDistributedComm" );

  if (myName == NULL || strlen(myName) == 0) {
    TCM_UnlockMasterMutex ( "TCM_EnableDistributedComm" );
    return TCM_Error;
  } else {
    thisAgentName = myName;
    if (CommInterface::initializeComm(myName, myHostMachine,
				      registerDistributedMessages) == SUCCESS) {
      TCM_SetExternalEventBypassHandler(CommInterface::eventHandlerForTCM);
      TCM_UnlockMasterMutex ( "TCM_EnableDistributedComm" );
      return TCM_Ok;
    } else {
      TCM_UnlockMasterMutex ( "TCM_EnableDistributedComm" );
      return TCM_Error;
    }
  }
}

TCM_Return_Type TCM_ConnectDistributedAgent (STRING agentName,
					     STRING hostMachine)
{
  TCM_LockMasterMutex ( "TCM_ConnectDistributedAgent" );

  if (!CommInterface::getCommHasBeenInitialized()) {
    tcmError("Need to call TCM_EnableDistributedComm first\n");
    TCM_UnlockMasterMutex ( "TCM_ConnectDistributedAgent" );
    return TCM_Error;
  } else if (agentConnectionIndex(agentName, -1) >= 0) {
    tcmWarning("Already connected to agent with name %s\n", agentName);
    TCM_UnlockMasterMutex ( "TCM_ConnectDistributedAgent" );
    return TCM_Error;
  } else if (CommInterface::openConnection(thisAgent(), 
					   hostMachine) != SUCCESS) {
    tcmError("Cannot connect to machine %s\n", hostMachine);
    TCM_UnlockMasterMutex ( "TCM_ConnectDistributedAgent" );
    return TCM_Error;
  } else {
    Agent_Connection conn;

    conn.agentName = agentName;
    conn.hostMachine = (hostMachine == NULL ? "localhost" : hostMachine);
    conn.index = CommInterface::getConnectionIndex(conn.hostMachine);
    agentConnections.insertLast(conn);
    TCM_UnlockMasterMutex ( "TCM_ConnectDistributedAgent" );
    return TCM_Ok;
  }
}

TCM_Return_Type TCM_RegisterDistributedException (STRING exceptionName, 
						  STRING exceptionDataFormat)
{
  return TCM_RegisterDistributedException(exceptionName, exceptionDataFormat,
					  create_TCM_Exception);
}

TCM_Return_Type 
TCM_RegisterDistributedException (STRING exceptionName, 
				 STRING exceptionDataFormat,
				 TCM_Exception::Creator creator)
{
  TCM_LockMasterMutex ( "TCM_RegisterDistributedException" );

  if (!CommInterface::getCommHasBeenInitialized()) {
    tcmError("Need to call TCM_EnableDistributedComm first\n");
    TCM_UnlockMasterMutex ( "TCM_RegisterDistributedException" );
    return TCM_Error;
  } else {
    CommInterface::registerMessage(exceptionMsgName(exceptionName),
				   exceptionDataFormat);
    distributedExceptionCreators[exceptionName] = creator;
    TCM_UnlockMasterMutex ( "TCM_RegisterDistributedException" );
    return TCM_Ok;
  }
}

STRING TCM_ThisAgent (void)
{
  TCM_LockMasterMutex ( "TCM_ThisAgent" );
  STRING returnValue = thisAgent();
  TCM_UnlockMasterMutex ( "TCM_ThisAgent" );
  return returnValue;
}
