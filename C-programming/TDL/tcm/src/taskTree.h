/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: taskTree.h
 *
 * ABSTRACT: Defines the hierarchy of task tree nodes and operations
 *           to create them, link them into a tree, and execute actions.
 *
 * EXPORTS:
 *
 * $Revision: 1.29 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: taskTree.h,v $
 * Revision 1.29  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.28  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.27  2003/10/18 12:45:23  reids
 * Fixed a memory leak
 *
 * Revision 1.26  2003/04/17 21:09:26  da0g
 * Added code to support [taskname] overloaded tasks.
 *
 * Revision 1.25  2003/01/29 20:30:23  da0g
 * Added userData to TaskTree.
 *
 * Revision 1.24  2002/06/26 16:49:45  reids
 * Made a distinction between the type-name and instance-name of a node.
 *  Enable instance name of node to be set.
 *  Insure warns that "malloc" (which strup uses) must be paired with "free".
 *
 * Revision 1.23  2002/04/02 17:25:29  reids
 * Fixed the problem with "delayTermination" of exception nodes -- now, nodes
 *   that are marked "delay termination" are terminated after they are
 *   achieved only if explicitly requested to be so.
 *
 * Revision 1.22  2002/03/26 05:19:53  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.21  2002/03/22 02:25:58  da0g
 * Removed USE_CACHED_LAST_CHILD code.
 * Added isVirtual() method.
 *
 * Revision 1.20  2002/01/18 14:20:17  reids
 * Upgraded tracing options
 *
 * Revision 1.19  2001/10/23 22:52:58  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.18  2001/07/24 12:49:03  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.17  2001/07/23 16:22:12  reids
 * removeRequestEvents => removeRequestEvent (just delete one event, not all).
 *
 * Revision 1.16  2001/06/11 15:56:10  reids
 * Fixed a problem with distributed nodes being confused about which node is
 *   the root node (basically made it possible to determine whether a virtual
 *   node is the root of some other tree).
 *
 * Revision 1.15  2001/04/04 14:26:12  reids
 * Task tree nodes are now garbage collected, by default, after they are
 *   completely achieved.  Can change this behavior using TCM_SetPersistence.
 * Also, cleaned up some memory leaks: Now seems to lose little, if any, memory
 *
 * Revision 1.14  2001/03/26 21:38:56  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.13  2000/01/19 21:26:42  reids
 * Added two new top-level functions:
 *   TCM_IsDoneHandling(ref) -- returns TRUE if the ref has raised success or
 * 			     failure.
 *   TCM_IsPostponed(ref) -- returns TRUE if the ref's action has finished
 * 		          executing, but the node has not yet raised
 * 			  success or failure.
 *
 * Revision 1.12  1999/08/04 14:00:19  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.11  1999/06/06 13:48:09  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.10  1998/12/16 03:08:19  reids
 * Added support for "on termination" functions.
 *   Also enabled tca.h and tcm.h to co-exist (needed to change values
 *   of several constants).
 *
 * Revision 1.9  98/09/15  18:45:22  da0g
 * Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
 * 
 * Revision 1.8  1998/08/05 10:57:26  reids
 * Made a compiler option for printing out debugging information.
 *   Created macros for TCM_TerminateAtAfter and TCM_DelayUntilAfter, for
 *   backwards compatibility.
 *
 * Revision 1.7  98/04/21  12:46:07  reids
 * Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
 *   particular event occurs.
 * Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
 *   a node by waiting some msecs after a particular event has occurred.
 * 
 * Revision 1.6  97/12/30  12:26:50  reids
 * Added flag to indicate whether node raised an exception (ie, failed).
 * 
 * Revision 1.5  97/12/29  17:06:29  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.4  97/12/22  16:53:01  reids
 * Basically, added "data" field for CALLBACK_ACTION,
 *  and started using "nodeData" for the activation data associated
 *  with monitors, and the failure data associated with exceptions.
 * 
 * Revision 1.3  97/12/18  00:21:47  reids
 * Changing ACTION_PTR to a handle, to aid in garbage collection.
 * 
 * Revision 1.2  97/12/04  17:50:19  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:38  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INCtaskTree
#define INCtaskTree

#include <stdio.h>
#include "tcmLogging.h"
#include "event.h"
#include "action.h"

#ifdef TRACE_UBER
#define TRACE
#endif

//#define TRACE
#ifdef TRACE
//#define TRACE_SIGNALS
#define TRACE_TRANSITIONS
#define TRACE_DELETIONS
#define TRACE_EXCEPTIONS
#define TRACE_TIMER
//#define TRACE_THREADS
#define TRACE_ACTION_DELETIONS
//#define TRACE_VIRTUAL
#define TRACE_AFTER_SIGNAL
#define TRACE_ACTIVATIONS
#endif

#ifdef DISTRIBUTED
typedef struct _Virtual_Address {
  STRING       host;
  STRING       creator;
  unsigned int id;
} Virtual_Address;

static inline void free_vaddress (const Virtual_Address &vaddress)
{
  // Insure warns that "malloc" (which strup uses) must be paired with "free"  
  free((void *)vaddress.host);
  free((void *)vaddress.creator);
}

#define DVIRTUAL virtual
#else
#define DVIRTUAL
#endif

inline BOOLEAN immediateSignal (Signal_Enum signal) 
{ return signal == Activate_Signal || signal == Start_Terminating_Signal; }

typedef tcmList<Task_Tree_Ref> Task_Tree_Node_List;
typedef List_Iterate<Task_Tree_Ref> Task_Tree_Node_Iterator;
typedef Const_List_Iterate<Task_Tree_Ref> Const_Task_Tree_Node_Iterator;

class Exception_Handler;  // Forward reference, to keep compiler happy
class Exception_Instance; // Forward reference, to keep compiler happy

typedef tcmHandle<Exception_Handler> Exception_Handler_Ref;

typedef tcmList<Exception_Handler_Ref> Exception_List;
typedef List_Iterate<Exception_Handler_Ref> Exception_Iterator;
typedef Const_List_Iterate<Exception_Handler_Ref> Const_Exception_Iterator;
typedef Exception_List *Exception_List_Ptr;
typedef Exception_List const *Const_Exception_List_Ptr;

typedef Task_Tree_Node *Task_Tree_Node_Ptr;

typedef tcmHandle<Ref_Count>  TCM_Generic_Ref;

class Task_Tree_Node : public Ref_Count, public Named_Object
{
public:
  Task_Tree_Node()
    { _initialize("Unknown"); _intraNodeConstraints(); }
  Task_Tree_Node(STRING nodeTypeName)
    { _initialize(nodeTypeName); _intraNodeConstraints(); }
  Task_Tree_Node(STRING nodeTypeName, const TCM_Action_Ref &nodeAction)
    { _initialize(nodeTypeName); _action = nodeAction; 
      _intraNodeConstraints(); }
  ~Task_Tree_Node() {
#ifdef TRACE_DELETIONS
#ifdef TRACE_UBER
    tcmMessage("Deleting %s {%d}\n", instanceName(), getNodeId());
#else
    tcmMessage("Deleting %s\n", instanceName());
#endif
#endif
#ifdef DISTRIBUTED
    if (validVirtualAddress()) free_vaddress(_vaddress);
#endif
    // Insure warns that "malloc" (which strup uses) must be paired with "free"
    if (_nodeTypeName) free((void *)_nodeTypeName);
  }

  Task_Tree_Ref getParent(void) const { return parent; }
  Exception_List_Ptr getExceptions(void) { return &exceptions; }
  Const_Exception_List_Ptr getExceptions(void) const { return &exceptions; }

  virtual BOOLEAN isGoal(void)      { return FALSE; }
  virtual BOOLEAN isException(void) { return FALSE; }
  virtual BOOLEAN isMonitor(void)   { return FALSE; }
  virtual BOOLEAN isVirtual(void)   { return FALSE; }

  DVIRTUAL BOOLEAN    validState(State_Enum state) const ;
  DVIRTUAL BOOLEAN    isCurrentState(State_Enum state) const ;
  DVIRTUAL State_Enum theCurrentState(State_Enum state) const
    { return currentState.current(state); }
  DVIRTUAL BOOLEAN    isChild(Task_Tree_Ref const &childNode) const 
    { return children.member(childNode); }

  DVIRTUAL void addParent(Task_Tree_Ref const &parentNode,
			  BOOLEAN canBeLastChild,
			  BOOLEAN addConstraints=TRUE);
  DVIRTUAL void addChild(Task_Tree_Ref const &childNode,
			 BOOLEAN addConstraints=TRUE);
  DVIRTUAL void removeChild(Task_Tree_Ref const &childRef)
    { children.removeItem(childRef); }

  DVIRTUAL Task_Tree_Node_Ptr firstChild(void) const;
  DVIRTUAL Task_Tree_Node_Ptr lastChild(BOOLEAN ignoreVeryLast) const;
  DVIRTUAL Task_Tree_Node_Ptr previousChild(Task_Tree_Ref const &child) const;
  DVIRTUAL Task_Tree_Node_Ptr nextChild(Task_Tree_Ref const &child) const;
  DVIRTUAL Task_Tree_Node_Ptr childNamed(STRING const name) const;
  DVIRTUAL Task_Tree_Node_Ptr ancestorNamed(STRING const name) const;

	/* This method must be thread-safe! */
  DVIRTUAL void execute (void)
  {
    TCM_LockMasterMutex   ( "taskTree:execute-start" );
    TCM_Action_Ref ourAction = _action;

    if (*ourAction)
    {
      TCM_UnlockMasterMutex ( "taskTree:execute-preExecute" );
      ourAction->execute(this);
      TCM_LockMasterMutex   ( "taskTree:execute-postExecute" );

      if (!_doneHandling)
	_postponed = TRUE;
    }
    else
    {
      tcmError("No action for node %s\n", instanceName());
    }

    TCM_UnlockMasterMutex ( "taskTree:execute-end" );
  }

  DVIRTUAL void signal(const Event &event);

  DVIRTUAL void addExpectedEvent(Signal_Enum signal,
				 Task_Tree_Ref const &signallingNode,
				 const void *timerData = NULL);
  DVIRTUAL void addRequestedEvent(State_Enum state, Signal_Enum signal,
				  Task_Tree_Ref const &signalledNode,
				  void const *data=NULL);
  DVIRTUAL void addRequestedEvent(State_Enum state,
				  SIMPLE_CALLBACK_FN_TYPE callbackFn,
				  const void *data);

  DVIRTUAL void removeExpectedEvent(Signal_Enum signal,
				    Task_Tree_Ref const &signallingNode);
  DVIRTUAL void removeRequestedEvent(Signal_Enum signal,
				     Task_Tree_Ref const &signalledNode);
  void removeRequestedEvent(const Event &event);
  DVIRTUAL void processRequestedEvents(void);
  DVIRTUAL void transitionTo(State_Enum newState);
  DVIRTUAL void doneHandling(void);

  DVIRTUAL void setFailed (void) { _failed = TRUE; }

  // Return TRUE if failed, FALSE otherwise
  DVIRTUAL BOOLEAN getNodeFailed (void) const { return _failed; }
  // Return TRUE if any node of the tree failed, FALSE otherwise
  DVIRTUAL BOOLEAN getTreeFailed (void) const;

  DVIRTUAL BOOLEAN terminate (void);
  DVIRTUAL BOOLEAN deallocate (void);
  DVIRTUAL void suspend (void);
  DVIRTUAL void unsuspend (void);
  DVIRTUAL BOOLEAN isSuspended (void) const { return _suspended; }

  DVIRTUAL BOOLEAN isDoneHandling (void) const { return _doneHandling; }
  DVIRTUAL BOOLEAN isPostponed    (void) const { return _postponed; }

  DVIRTUAL void delayTermination  (void) {
    if (!isTerminationDelayed()) _delayedTermination = Delay; }
  DVIRTUAL void enableTermination (void) { _delayedTermination = Nominal; }
  DVIRTUAL BOOLEAN isTerminationDelayed (void) const
    { return _delayedTermination != Nominal; }
  
  // Defined in tplConstr.cc
  void addConstraints(int tplConstraints);
  DVIRTUAL BOOLEAN addEventP (Signal_Enum signal, State_Enum state);

  // This is the type of node (goal, command, monitor, virtual, etc)
  virtual STRING className(void) const { return "Node"; }
  // This is a name specifying the node type (e.g. "goto")
  //   It can only be set when the node is allocated.
  STRING nodeTypeName(void) const { return _nodeTypeName; }
  // This is the name of the particular node in the task tree
  //   (e.g. "goto1").  Nodes can have the same nodeTypeName but
  //   different instanceNames.
  STRING instanceName(void) const { return getName(); }
    /* Virtual Nodes (Distributed Tasks) need to      *
     * distinguish between identically named nodes... */
  STRING getOverloadedNodeTypeNameIndex() const
				       { return _overloadedNodeTypeNameIndex; }
  void setOverloadedNodeTypeNameIndex( STRING theOverloadedNodeTypeNameIndex )
	     { _overloadedNodeTypeNameIndex = theOverloadedNodeTypeNameIndex; }


  int getNodeId (void) const { return id; }

  DVIRTUAL void setInstanceName (STRING newInstanceName)
    { if (name) free((void *)name);  name = strdup(newInstanceName); }
    
  DVIRTUAL void setNodeData (void const *nodeData) { _nodeData = nodeData; }
  DVIRTUAL void const *getNodeData (void) const { return _nodeData; }

  DVIRTUAL Ref_Count * getNodeRefCountData() const
    { return _nodeRefCountData . operator*();  }
  DVIRTUAL void setNodeRefCountData ( Ref_Count * theNodeRefCountData )
    { _nodeRefCountData = theNodeRefCountData; }

  DVIRTUAL void setUserData (void const *userData) { _userData = userData; }
  DVIRTUAL void const *getUserData (void) const { return _userData; }

  DVIRTUAL void setAction (TCM_Action_Ref action) { _action = action; }
  DVIRTUAL TCM_Action_Ref getAction (void) const { return _action; }

  // Functions to map between a task tree node and its id number
  int remember (void);
  Task_Tree_Node_Ptr recall (int refID) const;
  void forget (int refID);
  Task_Tree_Node_Ptr retrieve (int refID)
    { Task_Tree_Node_Ptr node = recall(refID); forget(refID); return node; }

  DVIRTUAL void displayTree (FILE *stream, int position = 0) const;

  DVIRTUAL void addTerminationAction (const TCM_Task_Tree_Ref &terminationRef);

  DVIRTUAL void findAndApplyExceptionHandler (Task_Tree_Ref parentNode,
					      Task_Tree_Ref failureNode,
					      Exception_Instance *exception);
  DVIRTUAL void noExceptionHandler (STRING failure) const;

  DVIRTUAL BOOLEAN getPersistence (void) const { return _persistent; }
  DVIRTUAL void setPersistence (BOOLEAN value);

  void cleanUp (void) { _destroy(); }

  void clearTimeouts (void) { _forceTimeouts(); }

  DVIRTUAL BOOLEAN isRootNode  (void) const;

 private: // Data items
  static int globalId;
  int id;
  State currentState;
  STRING _nodeTypeName;
  STRING _overloadedNodeTypeNameIndex; // Only used for Virtual nodes.
  Task_Tree_Ref parent;
  Task_Tree_Node_List children;
  BOOLEAN canBeLast;
  BOOLEAN _persistent; // FALSE (default) if to be deleted after achievement
  BOOLEAN _suspended;
  BOOLEAN _failed; // TRUE if this node raised an exception
  // TRUE if this node is done handling (raised success or failure)
  BOOLEAN _doneHandling;
  // TRUE if handler code has run, but task has not declared success or failure
  BOOLEAN _postponed;
  // Indicates whether this node should be terminated immediately when requested
  //   or if it should wait until achieved.  Used for "on termination" and
  //   exceptions.
  typedef enum {Nominal, Delay, Terminate} Delay_Term_Enum;
  Delay_Term_Enum _delayedTermination;

  Exception_List exceptions;
  
  Task_Tree_Node_List terminationRefs;

  static Task_Tree_Node_List memory;

 protected: // Data items
  Event_List expectedEvents;
  State_Event_List requestedEvents;
  TCM_Action_Ref    _action;
  void const      * _nodeData;
  TCM_Generic_Ref   _nodeRefCountData;
  void const      * _userData;

 protected: // Functions
  DVIRTUAL /*inline*/ BOOLEAN _noneExpected(Signal_Enum signal) const;
  // Different classes of nodes may handle the same signal in different ways.
  virtual void _handleSignal (Signal_Enum signal);
  virtual void _intraNodeConstraints (void);

 private: // Functions
   // Initialize task tree node data structure
  DVIRTUAL void _initialize(STRING nodeTypeName);
  DVIRTUAL void _processSignal (Signal_Enum signal) {
    // Handle the signal if no other signals of the given type are expected,
    //  or if the signal is to terminate the node (since this should happen
    //  if *any* event wants to terminate the node
    if (immediateSignal(signal) || _noneExpected(signal))
      _handleSignal(signal);
  }

  DVIRTUAL BOOLEAN _handleTransition (State_Enum newState);
  DVIRTUAL void _startTerminating (void);
  DVIRTUAL void _destroy (void);

  //  Unsuspend a single node, and trigger any waiting transitions
  DVIRTUAL void _unsuspendNode (void);
  
  DVIRTUAL STRING _nodeStatusString (void) const;

  DVIRTUAL void _forceTimeouts (void);

#ifdef DISTRIBUTED
 protected:
  Virtual_Address _vaddress;
  static Task_Tree_Node_List virtualNodes;
 public:
  BOOLEAN validVirtualAddress (void) const { return _vaddress.host != NULL; }
  const Virtual_Address &getVirtualAddress (void) const { return _vaddress; }
  void setVirtualAddress (STRING host, STRING creator, unsigned int id);
  void setVirtualAddress (const Virtual_Address &vaddress);

  virtual void distributedRemoveTimer(Signal_Enum signal,
				      STRING timerHost) const
    { tcmError("distributedRemoveTimer"); }

  // Functions to map between a task tree node and its virtual address
  void                      rememberVirtual (void);
  void                      forgetVirtual   (const Virtual_Address &vaddress);
  static Task_Tree_Node_Ptr recallVirtual   (const Virtual_Address &vaddress);
  static Task_Tree_Node_Ptr getVirtual      (const Virtual_Address &vaddress,
					     STRING nodeName = "Unknown");
#endif


/*************************/
/*** THREADING SUPPORT ***/
/*************************/
public:
  BOOLEAN getIsThreaded() const
    {
#ifdef THREADED
      return isThreaded;
#else /* THREADED */
      return FALSE;
#endif /* THREADED */
    }

  void setIsThreaded ( BOOLEAN theIsThreaded )
    { 
#ifdef THREADED
      isThreaded = theIsThreaded;
#else /* THREADED */
      theIsThreaded = FALSE; /* Eliminates any compiler warnings. */
      isThreaded    = FALSE; /* Eliminates any compiler warnings. */
#endif /* THREADED */
    }

private:
  BOOLEAN isThreaded;

}; /* class Task_Tree_Node */



class Task_Tree_Goal_Node : public Task_Tree_Node
{
 public:
  Task_Tree_Goal_Node(STRING nodeName);
  Task_Tree_Goal_Node(STRING nodeName, const TCM_Action_Ref &nodeAction);

  virtual STRING className(void) const { return "Goal"; }
  virtual BOOLEAN isGoal(void) { return TRUE; }

 protected:
  virtual void _handleSignal (Signal_Enum signal);
  virtual void _intraNodeConstraints (void);
};

class Task_Tree_Command_Node : public Task_Tree_Node
{
 public:
  Task_Tree_Command_Node(STRING nodeName);
  Task_Tree_Command_Node(STRING nodeName, const TCM_Action_Ref &nodeAction);
  
  virtual STRING className(void) const { return "Command"; }

 protected:
  virtual void _handleSignal (Signal_Enum signal);
  virtual void _intraNodeConstraints (void);
};

void addEventPair (Task_Tree_Ref const &signallingNode, State_Enum state,
		   Signal_Enum signal, Task_Tree_Ref const &signalledNode,
		   void const *data=NULL);

void taskTreeInsert (Task_Tree_Node_Ptr parent, Task_Tree_Node_Ptr child,
		     BOOLEAN isLastChild);

#endif /* INCtaskTree */
