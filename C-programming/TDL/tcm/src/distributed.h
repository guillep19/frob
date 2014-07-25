/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1999 Reid Simmons.  All rights reserved.
 *
 * FILE: distributed.h
 *
 * ABSTRACT: Functions for creating distributed task tree nodes using
 *           message-passing protocols.
 *
 * $Revision: 1.10 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: distributed.h,v $
 * Revision 1.10  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.9  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.8  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.7  2003/04/17 21:09:26  da0g
 * Added code to support [taskname] overloaded tasks.
 *
 * Revision 1.6  2002/06/26 16:49:06  reids
 * Enable instance name of node to be set in a distributed fashion.
 *
 * Revision 1.5  2002/03/26 05:19:52  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.4  2001/03/26 21:38:56  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.3  2001/02/27 02:37:03  trey
 * added client data to action and allocation callbacks; made communications go to the main IPC context by default when we don't explicitly connect
 *
 * Revision 1.2  2001/02/17 03:29:25  reids
 * Improved distributed version of TCM.  No longer does each agent have to
 *  connect to a separate server.  Also, simplified the communications
 *  interface since IPC now (version 3.4) supports automatic unmarshalling of
 *  data when invoking message handlers.
 *
 * Revision 1.1  1999/08/04 14:00:17  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 **************************************************************************/

#ifndef INCdistributed
#define INCdistributed

#include "taskTree.h"

#define USE_STDOUT (1)
#define USE_STDERR (2)

#define IS_DISTRIBUTED(node) ((node)->validVirtualAddress() && !streq((node)->getVirtualAddress().host, thisAgent()))

#define VNODE_AGENT_NAME(node) ((node)->getVirtualAddress().host)
#define VNODE_HOST_INDEX(node) agentConnectionIndex(VNODE_AGENT_NAME(node))
#define VNODE_MSG_NAME(msgName,node) \
  agentMessage(msgName, VNODE_AGENT_NAME(node))

class Agent_Connection
{
 public:
  STRING agentName;
  STRING hostMachine;
  int    index;
  Agent_Connection() : agentName(""), hostMachine("localhost"), index(-1) {}

  BOOLEAN operator== (const Agent_Connection &conn2) const 
    { return !strcmp(agentName, conn2.agentName); }
};

typedef tcmList<Agent_Connection> Agent_Connection_List;
typedef Const_List_Iterate<Agent_Connection> Const_Agent_Connection_Iterator;

class Distributed_Task
{
 public:
  STRING taskName;
  STRING overloadedTaskNameIndex;
  TASK_ALLOCATION_FN allocationFn;
  ACTION_CREATION_FN actionFn;
  void *allocationFnClientData;
  void *actionFnClientData;

  BOOLEAN operator== (const Distributed_Task & task2) const
    { return ( streq(taskName, task2.taskName) )
	 &&  ( streq(overloadedTaskNameIndex, task2.overloadedTaskNameIndex));}

  BOOLEAN equals ( STRING theTaskName,
		   STRING theOverloadedTaskNameIndex )
    { return ( streq(taskName,                theTaskName                ) )
	 &&  ( streq(overloadedTaskNameIndex, theOverloadedTaskNameIndex ) ); }
};

typedef tcmList<Distributed_Task> Distributed_Task_List;
typedef Const_List_Iterate<Distributed_Task> Const_Distributed_Task_Iterator;

#define SIGNAL_FORMAT "{enum:12}"
#define STATE_FORMAT  "{enum:13}"
#define TIME_FORMAT   "long"
#define VADDRESS_FORMAT "{string, string, int}"

typedef struct {
  Virtual_Address vaddress;
} SIMPLE_VADDRESS_MSG_TYPE, *SIMPLE_VADDRESS_MSG_PTR;

#define SIMPLE_VADDRESS_FORMAT "{" VADDRESS_FORMAT "}"

// Allocate Message

typedef struct {
  Virtual_Address vaddress;
  STRING          taskName;
  STRING          overloadedTaskNameIndex;
} ALLOCATE_MSG_TYPE, *ALLOCATE_MSG_PTR;

#define ALLOCATE_MSG        "Allocate_Task_Msg"
#define ALLOCATE_MSG_FORMAT "{" VADDRESS_FORMAT ", string , string }"

// Set Action Message Template

typedef struct {
  Virtual_Address vaddress;
  const void *args;
} SET_ACTION_MSG_TYPE, *SET_ACTION_MSG_PTR;

#define SET_ACTION_MSG_PREFIX    "Set_Action_"
#define SET_ACTION_FORMAT_PREFIX VADDRESS_FORMAT

#define EXCEPTION_MSG_PREFIX "Exception_"

// Add Parent Message

typedef struct {
  Virtual_Address parentAddress;
  Virtual_Address childAddress;
  BOOLEAN canBeLast;
} ADD_PARENT_MSG_TYPE, *ADD_PARENT_MSG_PTR;

#define ADD_PARENT_MSG        "Add_Parent_Msg"
#define ADD_PARENT_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT ", boolean}"

// Add Child Message

typedef struct {
  Virtual_Address parentAddress;
  Virtual_Address childAddress;
} ADD_CHILD_MSG_TYPE, *ADD_CHILD_MSG_PTR;

#define ADD_CHILD_MSG        "Add_Child_Msg"
#define ADD_CHILD_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT "}"

// Remove Child Message

typedef ADD_CHILD_MSG_TYPE REMOVE_CHILD_MSG_TYPE, *REMOVE_CHILD_MSG_PTR;

#define REMOVE_CHILD_MSG        "Remove_Child_Msg"
#define REMOVE_CHILD_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT "}"

// Signal Message

typedef struct {
  Virtual_Address signalledAddress;
  Virtual_Address signallingAddress;
  Signal_Enum signal;
} SIGNAL_MSG_TYPE, *SIGNAL_MSG_PTR;

#define SIGNAL_MSG        "Signal_Msg"
#define SIGNAL_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT ", " SIGNAL_FORMAT "}"

// Add Expected Event Message

typedef struct {
  Virtual_Address signalledAddress;
  Virtual_Address signallingAddress;
  Signal_Enum signal;
  STRING timerData;
} ADD_EXPECTED_MSG_TYPE, *ADD_EXPECTED_MSG_PTR;

#define ADD_EXPECTED_MSG        "Add_Expected_Msg"
#define ADD_EXPECTED_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT ", " SIGNAL_FORMAT ", string}"

// Add Requested Event Message

typedef struct _ADD_REQUESTED_MSG_TYPE {
  Virtual_Address signallingAddress;
  Virtual_Address signalledAddress;
  Signal_Enum signal;
  State_Enum state;
} ADD_REQUESTED_MSG_TYPE, *ADD_REQUESTED_MSG_PTR;

#define ADD_REQUESTED_MSG        "Add_Requested_Msg"
#define ADD_REQUESTED_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT ", " SIGNAL_FORMAT ", " STATE_FORMAT "}"

// Add Requested Callback Message

typedef struct {
  Virtual_Address signallingAddress;
  State_Enum state;
  STRING host;
} ADD_REQUESTED_CALLBACK_MSG_TYPE, *ADD_REQUESTED_CALLBACK_MSG_PTR;

#define ADD_REQUESTED_CALLBACK_MSG        "Add_Requested_Callback_Msg"
#define ADD_REQUESTED_CALLBACK_MSG_FORMAT "{" VADDRESS_FORMAT ", " STATE_FORMAT ", string}"

// Invoke Callback Message

typedef struct {
  Virtual_Address signallingAddress;
  State_Enum state;
  // This is a kludge, but needed for handlig distributed "after X" constraints
  BOOLEAN isTerminating;
} INVOKE_CALLBACK_MSG_TYPE, *INVOKE_CALLBACK_MSG_PTR;

#define INVOKE_CALLBACK_MSG        "Invoke_Callback_Msg"
#define INVOKE_CALLBACK_MSG_FORMAT  "{" VADDRESS_FORMAT ", " STATE_FORMAT ", boolean}"

// Remove Expected Event Message

typedef SIGNAL_MSG_TYPE REMOVE_EXPECTED_MSG_TYPE, *REMOVE_EXPECTED_MSG_PTR;

#define REMOVE_EXPECTED_MSG        "Remove_Expected_Msg"
#define REMOVE_EXPECTED_MSG_FORMAT SIGNAL_MSG_FORMAT 

// Remove Requested Event Message

typedef SIGNAL_MSG_TYPE REMOVE_REQUESTED_MSG_TYPE, *REMOVE_REQUESTED_MSG_PTR;

#define REMOVE_REQUESTED_MSG        "Remove_Requested_Msg"
#define REMOVE_REQUESTED_MSG_FORMAT SIGNAL_MSG_FORMAT 

// Remove Timer Message

typedef struct {
  Virtual_Address vaddress;
  Signal_Enum signal;
} REMOVE_TIMER_MSG_TYPE, *REMOVE_TIMER_MSG_PTR;

#define REMOVE_TIMER_MSG        "Remove_Timer_Msg"
#define REMOVE_TIMER_MSG_FORMAT "{" VADDRESS_FORMAT ", " SIGNAL_FORMAT "}"

// Done Handling Message

typedef SIMPLE_VADDRESS_MSG_TYPE DONE_HANDLING_MSG_TYPE, *DONE_HANDLING_MSG_PTR;

#define DONE_HANDLING_MSG        "Done_Handling_Msg"
#define DONE_HANDLING_MSG_FORMAT SIMPLE_VADDRESS_FORMAT

// Terminate Message

typedef SIMPLE_VADDRESS_MSG_TYPE TERMINATE_MSG_TYPE, *TERMINATE_MSG_PTR;

#define TERMINATE_MSG        "Terminate_Msg"
#define TERMINATE_MSG_FORMAT SIMPLE_VADDRESS_FORMAT

// Deallocate Message

typedef SIMPLE_VADDRESS_MSG_TYPE DEALLOCATE_MSG_TYPE, *DEALLOCATE_MSG_PTR;

#define DEALLOCATE_MSG        "Deallocate_Msg"
#define DEALLOCATE_MSG_FORMAT SIMPLE_VADDRESS_FORMAT

// Suspend Message

typedef SIMPLE_VADDRESS_MSG_TYPE SUSPEND_MSG_TYPE, *SUSPEND_MSG_PTR;

#define SUSPEND_MSG        "Suspend_Msg"
#define SUSPEND_MSG_FORMAT SIMPLE_VADDRESS_FORMAT

// Unsuspend Message

typedef SIMPLE_VADDRESS_MSG_TYPE UNSUSPEND_MSG_TYPE, *UNSUSPEND_MSG_PTR;

#define UNSUSPEND_MSG        "Unsuspend_Msg"
#define UNSUSPEND_MSG_FORMAT SIMPLE_VADDRESS_FORMAT

// DelayTermination Message

typedef SIMPLE_VADDRESS_MSG_TYPE  DELAY_TERMINATION_MSG_TYPE;
typedef SIMPLE_VADDRESS_MSG_TYPE *DELAY_TERMINATION_MSG_PTR;

#define DELAY_TERMINATION_MSG        "Delay_Termination_Msg"
#define DELAY_TERMINATION_MSG_FORMAT SIMPLE_VADDRESS_FORMAT

// EnableTermination Message

typedef SIMPLE_VADDRESS_MSG_TYPE  ENABLE_TERMINATION_MSG_TYPE;
typedef SIMPLE_VADDRESS_MSG_TYPE *ENABLE_TERMINATION_MSG_PTR;

#define ENABLE_TERMINATION_MSG        "Enable_Termination_Msg"
#define ENABLE_TERMINATION_MSG_FORMAT SIMPLE_VADDRESS_FORMAT

// Add Termination Message

typedef struct {
  Virtual_Address vaddress;
  Virtual_Address terminationAddress;
  STRING terminationName;
} ADD_TERMINATION_MSG_TYPE, *ADD_TERMINATION_MSG_PTR;

#define ADD_TERMINATION_MSG        "Add_Termination_Action_Msg"
#define ADD_TERMINATION_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT ", string}"

// SetInstanceName Message

typedef struct {
  Virtual_Address vaddress;
  STRING instanceName;
} SET_INSTANCE_NAME_MSG_TYPE, *SET_INSTANCE_NAME_MSG_PTR;

#define SET_INSTANCE_NAME_MSG        "Set_Instance_Name_Action_Msg"
#define SET_INSTANCE_NAME_MSG_FORMAT "{" VADDRESS_FORMAT ", string}"

// Display Tree Message

typedef struct {
  Virtual_Address vaddress;
  int whichStream;
  int position;
} DISPLAY_TREE_MSG_TYPE, *DISPLAY_TREE_MSG_PTR;

#define DISPLAY_TREE_MSG        "Display_Tree_Msg"
#define DISPLAY_TREE_MSG_FORMAT "{" VADDRESS_FORMAT ", int, int}"

// Find Exception Handler Message

typedef struct {
  int length;
  void *data;
} EXCEPTION_DATA_TYPE, *EXCEPTION_DATA_PTR;

#define EXCEPTION_DATA_FORMAT "{int, <byte:1>}"

typedef struct {
  Virtual_Address contextNode;
  Virtual_Address parentNode;
  Virtual_Address failureNode;
  STRING failure;
  EXCEPTION_DATA_TYPE exceptionData;
} FIND_EXCEPTION_HANDLER_MSG_TYPE, *FIND_EXCEPTION_HANDLER_MSG_PTR;

#define FIND_EXCEPTION_HANDLER_MSG        "Find_Exception_Handler_Msg"
#define FIND_EXCEPTION_HANDLER_MSG_FORMAT "{" VADDRESS_FORMAT ", " VADDRESS_FORMAT ", " VADDRESS_FORMAT ", string, " EXCEPTION_DATA_FORMAT "}"

// No Exception Message

typedef struct {
  Virtual_Address failureNode;
  STRING failure;
} NO_EXCEPTION_HANDLER_MSG_TYPE, *NO_EXCEPTION_HANDLER_MSG_PTR;

#define NO_EXCEPTION_HANDLER_MSG        "No_Exception_Handler_Msg"
#define NO_EXCEPTION_HANDLER_MSG_FORMAT "{" VADDRESS_FORMAT ", string}"

typedef struct {
  Virtual_Address nodeRef;
  BOOLEAN value;
} SET_PERSISTENCE_MSG_TYPE, *SET_PERSISTENCE_MSG_PTR;

#define SET_PERSISTENCE_MSG        "Set_Persistence_Msg"
#define SET_PERSISTENCE_MSG_FORMAT "{" VADDRESS_FORMAT ", boolean}"

STRING thisAgent (void);

void distributedAddParent (const Task_Tree_Ref &childNode,
			   const Task_Tree_Ref &parentNode,
			   BOOLEAN canBeLast);

void distributedAddChild (const Task_Tree_Ref &parentNode,
			  const Task_Tree_Ref &childNode);

void distributedRemoveChild (const Task_Tree_Ref &parentNode,
			     const Task_Tree_Ref &childNode);

void distributedSignal (const Task_Tree_Ref &signalledNode,
			const Event &event);

void distributedAddExpectedEvent (const Task_Tree_Ref &signalledNode,
				  Signal_Enum signal,
				  const Task_Tree_Ref &signallingNode,
				  const void *timerData);

void distributedAddRequestedEvent (const Task_Tree_Ref &signallingNode,
				   State_Enum state, Signal_Enum signal,
				   const Task_Tree_Ref &signalledNode);

void distributedAddRequestedCallback (const Task_Tree_Ref &signallingNode,
				      State_Enum state);

void distributedInvokeCallback (const Task_Tree_Ref &signallingNode,
				State_Enum state, STRING host);

void distributedRemoveExpectedEvent (const Task_Tree_Ref &signalledNode,
				     Signal_Enum signal,
				     const Task_Tree_Ref &signallingNode);

void distributedRemoveRequestedEvents (const Task_Tree_Ref &signallingNode,
				       Signal_Enum signal,
				       const Task_Tree_Ref &signalledNode);

void distributedRemoveTimer (const Task_Tree_Ref &nodeRef, Signal_Enum signal,
			     STRING timerHost);

void distributedDoneHandling (const Task_Tree_Ref &nodeRef);

void distributedTerminate  (const Task_Tree_Ref &nodeRef);
void distributedDeallocate (const Task_Tree_Ref &nodeRef);
void distributedSuspend    (const Task_Tree_Ref &nodeRef);
void distributedUnsuspend  (const Task_Tree_Ref &nodeRef);

void distributedDelayTermination  (const Task_Tree_Ref &nodeRef);
void distributedEnableTermination (const Task_Tree_Ref &nodeRef);

void distributedAddTermination (const Task_Tree_Ref &nodeRef,
				const Task_Tree_Ref &terminationNode);

void distributedSetInstanceName(const Task_Tree_Ref &nodeRef,
				STRING newInstanceName);

void distributedDisplayTree (const Task_Tree_Ref &nodeRef,
			     int whichStream, int position);

void distributedFindAndApplyExceptionHandler (const Task_Tree_Ref &nodeRef,
					      const Task_Tree_Ref &parentNode,
					      const Task_Tree_Ref &failureNode,
					      Exception_Instance *exception);

void distributedNoExceptionHandler (const Task_Tree_Ref &nodeRef,
				    STRING failure);

void distributedCheckTimedEventAfterSignal (const Task_Tree_Ref afterRef, 
					    const Task_Tree_Ref nodeRef, 
					    Signal_Enum signal,
					    State_Enum state);

void distributedSetPersistence (const Task_Tree_Ref nodeRef, BOOLEAN value);

#endif // INCdistributed
