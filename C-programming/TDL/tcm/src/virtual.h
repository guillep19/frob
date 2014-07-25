/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1999 Reid Simmons.  All rights reserved.
 *
 * FILE: virtual.h
 *
 * ABSTRACT: Defines "virtual" task tree nodes used for distributing a
 *           single task tree amongst multiple processes.  A "virtual" node
 *           has "real" counterpart in another process.  "Virtual" nodes 
 *           know how to send messages to their real counterparts and how
 *           to interpret messages sent to them.
 *
 *   Example:      Process 1      Process 2
 *                   [A]             [A']
 *                   / \               \
 *                 [B] [C']            [C]
 *
 *       In process 1, nodes A and B are real, and C' is virtual;
 *       In process 2, node C is real and node A' is virtual.  Virtual
 *       Virtual node C' corresponds to real node C, and A' corresponds to A.
 *
 * $Revision: 1.12 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: virtual.h,v $
 * Revision 1.12  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.11  2008/07/11 15:47:03  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.10  2002/06/26 16:50:12  reids
 * Made a distinction between the type-name and instance-name of a node.
 *  Enable instance name of node to be set.
 *
 * Revision 1.9  2002/03/26 05:19:54  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.8  2002/03/22 02:26:14  da0g
 * Added isVirtual() method.
 *
 * Revision 1.7  2002/02/05 17:45:18  reids
 * Backed out the getLocal function for distributed nodes -- instead,
 *   "getVirtual" creates a new virtual node only if the "host" given as the
 *   virtual address is not the current agent (o/w the node is "local").
 * Fixed several bugs relating to race conditions in the distributed version.
 *
 * Revision 1.6  2002/01/18 14:18:17  reids
 * Added "getLocal" to prevent race condition bugs with deleted virtual nodes
 *
 * Revision 1.5  2001/08/07 23:51:53  da0g
 * Fixed:  Renamed removeRequestedEvents to removeRequestedEvent.
 *
 * Revision 1.4  2001/07/24 12:49:05  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.3  2001/06/11 15:56:10  reids
 * Fixed a problem with distributed nodes being confused about which node is
 *   the root node (basically made it possible to determine whether a virtual
 *   node is the root of some other tree).
 *
 * Revision 1.2  2000/01/19 21:26:43  reids
 * Added two new top-level functions:
 *   TCM_IsDoneHandling(ref) -- returns TRUE if the ref has raised success or
 * 			     failure.
 *   TCM_IsPostponed(ref) -- returns TRUE if the ref's action has finished
 * 		          executing, but the node has not yet raised
 * 			  success or failure.
 *
 * Revision 1.1  1999/08/04 14:00:20  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 **************************************************************************/

#ifndef INCvirtual
#define INCvirtual

#include "taskTree.h"

typedef class Virtual_Node *Virtual_Node_Ptr;

class Virtual_Node : public Task_Tree_Node
{
 public:
  Virtual_Node (STRING taskName, STRING host, STRING creator)
    : Task_Tree_Node(taskName)
    { setVirtualAddress(host, creator, getNodeId()); _initialize(); }
  Virtual_Node (STRING taskName, const Virtual_Address &vaddress)
    : Task_Tree_Node(taskName)
    { setVirtualAddress(vaddress); _initialize(); }
  ~Virtual_Node () { 
#ifdef TRACE_DELETIONS
    tcmMessage("Killing %s (%s/%s/%d)\n", instanceName(), _vaddress.host,
	       _vaddress.creator, _vaddress.id);
#endif
  }

  virtual BOOLEAN isGoal(void)      { tcmError("isGoal\n"); return FALSE; }
  virtual BOOLEAN isException(void) { tcmError("isException\n");return FALSE; }
  virtual BOOLEAN isMonitor(void)   { tcmError("isMonitor\n"); return FALSE; }
  virtual BOOLEAN isVirtual(void)   { return TRUE; }

  virtual BOOLEAN validState(State_Enum state) const 
    { tcmError("validState\n"); return FALSE; }
  virtual BOOLEAN    isCurrentState(State_Enum state) const 
    { tcmError("isCurrentState\n"); return FALSE; }
  virtual State_Enum theCurrentState(State_Enum state) const
    { tcmError("theCurrentState\n"); return Unknown_State; }
  virtual BOOLEAN    isChild(Task_Tree_Ref const &childNode) const 
    { tcmError("isChild\n"); return FALSE; }

  virtual void addParent(Task_Tree_Ref const &parentNode,
			 BOOLEAN canBeLastChild, BOOLEAN addConstraints);
  virtual void addChild(Task_Tree_Ref const &childNode, BOOLEAN addConstraints);
  virtual void removeChild(Task_Tree_Ref const &childNode);

  virtual  Task_Tree_Node_Ptr firstChild(void) const
    { tcmError("firstChild\n"); return NULL; }
  virtual  Task_Tree_Node_Ptr lastChild(BOOLEAN ignoreVeryLast) const
    { tcmError("lastChild\n"); return NULL; }
  virtual  Task_Tree_Node_Ptr previousChild(Task_Tree_Ref const &child) const
    { tcmError("previousChild\n"); return NULL; }
  virtual  Task_Tree_Node_Ptr nextChild(Task_Tree_Ref const &child) const
    { tcmError("nextChild\n"); return NULL; }
  virtual  Task_Tree_Node_Ptr childNamed(STRING const name) const
    { tcmError("childNamed\n"); return NULL; }
  virtual  Task_Tree_Node_Ptr ancestorNamed(STRING const name) const
    { tcmError("ancestorNamed\n"); return NULL; }

  virtual  void execute (void) { tcmError("execute\n"); }
  virtual  void signal(const Event &event);

  virtual  void addExpectedEvent(Signal_Enum signal,
				 Task_Tree_Ref const &signallingNode,
				 const void *timerData = NULL);
  virtual  void addRequestedEvent(State_Enum state, Signal_Enum signal,
				  Task_Tree_Ref const &signalledNode,
				  const void *data=NULL);
  virtual  void addRequestedEvent(State_Enum state, 
				  SIMPLE_CALLBACK_FN_TYPE callbackFn, 
				  const void *data);
  void          invokeCallback (State_Enum state);

  virtual  void removeExpectedEvent(Signal_Enum signal,
				    Task_Tree_Ref const &signallingNode);
  virtual  void removeRequestedEvent(Signal_Enum signal,
				      Task_Tree_Ref const &signalledNode);
  virtual  void processRequestedEvents(void)
    { tcmError("processRequestedEvents\n"); }
  virtual  void transitionTo(State_Enum newState) { tcmError("transitionTo\n"); }
  virtual  void doneHandling(void);

  virtual  void setFailed (void) { tcmError("setFailed\n"); }

  // Return TRUE if failed, FALSE otherwise
  virtual  BOOLEAN getNodeFailed (void) const 
    { tcmError("getNodeFailed\n"); return FALSE; }
  // Return TRUE if any node of the tree failed, FALSE otherwise
  virtual  BOOLEAN getTreeFailed (void) const 
    { tcmError("getTreeFailed\n"); return FALSE; }

  virtual  BOOLEAN terminate (void);
  virtual  BOOLEAN deallocate (void);
  virtual  void suspend (void);
  virtual  void unsuspend (void);
  virtual  BOOLEAN isSuspended (void) const
    { tcmError("isSuspended\n"); return FALSE; }

  virtual  BOOLEAN isDoneHandling (void) const 
    { tcmError("isDoneHandling\n"); return FALSE; }
  virtual  BOOLEAN isPostponed (void) const
    { tcmError("isPostponed\n"); return FALSE; }

  virtual void delayTermination  (void);
  virtual void enableTermination (void);
  virtual BOOLEAN isTerminationDelayed (void) const 
    { tcmError("isTerminationDelayed\n"); return FALSE; }    

  virtual BOOLEAN addEventP (Signal_Enum signal, State_Enum state);

  virtual STRING className(void) const { return "Virtual"; }

  virtual void setInstanceName (STRING newInstanceName);

  virtual void setNodeData (void const *nodeData) { tcmError("setNodeData\n");}
  virtual void const *getNodeData (void) const
    { tcmError("getNodeData\n"); return NULL; }

  virtual Ref_Count * getNodeRefCountData() const
    { tcmError("getNodeRefCountData\n"); return NULL; }
  virtual void setNodeRefCountData ( Ref_Count * theNodeRefCountData )
    { tcmError("setNodeRefCountData\n");}

  virtual void setAction (TCM_Action_Ref action) { tcmError("setAction\n"); }
  virtual TCM_Action_Ref getAction (void) const 
    { tcmError("getAction\n"); return NULL; }

  virtual  void displayTree (FILE *stream, int position = 0) const;

  virtual  void addTerminationAction (const TCM_Task_Tree_Ref &terminationRef);

  virtual void findAndApplyExceptionHandler (Task_Tree_Ref parentNode,
					     Task_Tree_Ref failureNode,
					     Exception_Instance *exception);
  virtual void noExceptionHandler (STRING failure) const;

  virtual BOOLEAN getPersistence (void) const { 
    tcmError("getPersistence\n"); return FALSE; }
  virtual void setPersistence (BOOLEAN value);

  virtual BOOLEAN isRootNode  (void) const;

 protected: // Functions
  virtual  inline BOOLEAN _noneExpected(Signal_Enum signal) const 
    { tcmError("_noneExpected\n"); return FALSE; }
  // Different classes of nodes may handle the same signal in different ways.
  virtual void _handleSignal (Signal_Enum signal)
    { tcmError("_handleSignal\n"); }
  virtual void _intraNodeConstraints (void) 
    { tcmError("_intraNodeConstraints\n"); }

 private: // Functions
   // Initialize task tree node data structure
  virtual  void _initialize(void);
  virtual  void _processSignal (Signal_Enum signal) 
    { tcmError("_processSignal\n"); }

  virtual  BOOLEAN _handleTransition (State_Enum newState)
    { tcmError("_handleTransition\n"); return FALSE; }
  virtual  void _startTerminating (void) { tcmError("_startTerminating\n"); }
  virtual  void _destroy (void) { tcmError("_destroy\n"); }

  //  Unsuspend a single node, and trigger any waiting transitions
  virtual  void _unsuspendNode (void) { tcmError("_unsuspendNode\n"); }
  
  virtual  STRING _nodeStatusString (void) const 
    { tcmError("_nodeStatusString\n"); return "Unknown"; }

  virtual void _forceTimeouts (void) { tcmError("_forceTimeouts\n"); }

  virtual void distributedRemoveTimer (Signal_Enum signal,
				       STRING timerHost) const;
  // This is a kludge, but needed for handlig distributed "after X" constraints
  BOOLEAN _isTerminating;
 public:
  void setIsTerminating (BOOLEAN value) { _isTerminating = value; }
  BOOLEAN getIsTerminating () const { return _isTerminating; }
};


#endif // INCvirtual
