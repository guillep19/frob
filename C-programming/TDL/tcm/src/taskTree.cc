/**************************************************************************
 * 
 * PROJECT: Task Control Management
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: taskTree.cc
 *
 * ABSTRACT: Defines the hierarchy of task tree nodes and operations
 *           to create them, link them into a tree, and execute actions.
 *
 * EXPORTS:
 *
 * $Revision: 1.40 $
 * $Date: 2009/05/04 19:44:49 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: taskTree.cc,v $
 * Revision 1.40  2009/05/04 19:44:49  reids
 * Changed to using snprintf to avoid corrupting the stack on overflow
 *
 * Revision 1.39  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.38  2009/01/06 18:10:11  reids
 * Fixed compiler warning -- had been doing a really nasty cast.
 *
 * Revision 1.37  2008/07/11 15:47:02  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.36  2003/04/17 21:09:26  da0g
 * Added code to support [taskname] overloaded tasks.
 *
 * Revision 1.35  2003/01/29 20:30:22  da0g
 * Added userData to TaskTree.
 *
 * Revision 1.34  2002/06/26 16:50:11  reids
 * Made a distinction between the type-name and instance-name of a node.
 *  Enable instance name of node to be set.
 *
 * Revision 1.33  2002/06/10 18:50:30  reids
 * Another try at fixing the "postponed" bug -- the previous fix did not
 *   work if the node that was postponed was a descendant of the one being
 *   terminated.  The new version works correctly.
 *
 * Revision 1.32  2002/06/07 20:56:24  reids
 * Fixed a bug where "postponed" tasks could not be terminated (since they
 *  had not officially completed handling).  Now, "terminate" forces postponed
 *  tasks to be "done handling", so they can be terminated normally.
 *
 * Revision 1.31  2002/04/02 17:25:29  reids
 * Fixed the problem with "delayTermination" of exception nodes -- now, nodes
 *   that are marked "delay termination" are terminated after they are
 *   achieved only if explicitly requested to be so.
 *
 * Revision 1.30  2002/03/26 05:19:52  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.29  2002/03/22 02:25:01  da0g
 * Removed USE_CACHED_LAST_CHILD code.
 *
 * Revision 1.28  2002/02/05 17:45:17  reids
 * Backed out the getLocal function for distributed nodes -- instead,
 *   "getVirtual" creates a new virtual node only if the "host" given as the
 *   virtual address is not the current agent (o/w the node is "local").
 * Fixed several bugs relating to race conditions in the distributed version.
 *
 * Revision 1.27  2002/01/18 14:21:29  reids
 * Handling signals from timers better;
 *   Commented out use of _cachedLastChild;
 *   Delay "forgetVirtual" to help prevent race conditions
 *
 * Revision 1.26  2001/11/20 19:21:51  reids
 * Moved a few definitions around (forgetVirtual and virtualNodes) so that one
 *  can compile TCM with the -DDISTRIBUTED flag, but then link against it
 *  without having to incorporate any of the distributed files (which, in
 *  turn, need IPC and ipcInterface).
 *
 * Revision 1.25  2001/10/23 22:52:58  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.24  2001/09/07 03:45:02  reids
 * Refixed the "ConstrainWhenAfter" bug -- actually deleting the data if the
 *   signal is terminated before being sent.
 * Also NULL'ed out a task's action reference when the node is terminated.
 *   Previously, the action was deleted *after* the node was deleted.  But, if
 *   the action had an internal reference to the task itself, this would
 *   create a circular dependency in the reference counts, and the node (and
 *   hence the action) would never actually be freed.  Setting the action to
 *   NULL explicitly breaks this chain.
 *
 * Revision 1.23  2001/09/07 02:48:05  reids
 * Fixed two minor, but annoying bugs:
 *  1) Monitors that were terminated with TCM_TerminateNode did not, in fact,
 *     ever really terminate.
 *  2) "ConstrainWhenAfter" signals that were deleted before running did not
 *     release
 *
 * Revision 1.22  2001/08/07 23:51:53  da0g
 * Fixed:  Renamed removeRequestedEvents to removeRequestedEvent.
 *
 * Revision 1.21  2001/07/24 12:49:03  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.20  2001/07/23 16:22:12  reids
 * removeRequestEvents => removeRequestEvent (just delete one event, not all).
 *
 * Revision 1.19  2001/06/15 19:00:48  reids
 * Fixed a bug that David Apfelbaum found having to do with destroying a node
 *   that had a "constrainWhenAfter" event attached to it.
 *
 * Revision 1.18  2001/06/11 21:39:23  reids
 * Remove destroyed/cleaned-up nodes from the "virtual nodes" list, so they
 *  can be garbage collected.
 *
 * Revision 1.17  2001/06/11 15:56:09  reids
 * Fixed a problem with distributed nodes being confused about which node is
 *   the root node (basically made it possible to determine whether a virtual
 *   node is the root of some other tree).
 *
 * Revision 1.16  2001/04/04 14:26:12  reids
 * Task tree nodes are now garbage collected, by default, after they are
 *   completely achieved.  Can change this behavior using TCM_SetPersistence.
 * Also, cleaned up some memory leaks: Now seems to lose little, if any, memory
 *
 * Revision 1.15  2000/01/19 21:26:42  reids
 * Added two new top-level functions:
 *   TCM_IsDoneHandling(ref) -- returns TRUE if the ref has raised success or
 * 			     failure.
 *   TCM_IsPostponed(ref) -- returns TRUE if the ref's action has finished
 * 		          executing, but the node has not yet raised
 * 			  success or failure.
 *
 * Revision 1.14  1999/08/04 14:00:18  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.13  1999/06/06 13:48:08  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.12  1998/12/16 03:08:19  reids
 * Added support for "on termination" functions.
 *   Also enabled tca.h and tcm.h to co-exist (needed to change values
 *   of several constants).
 *
// Revision 1.11  98/10/20  22:06:25  reids
// Don't add Start_Terminating signals if signalled node is already completed.
// 
// Revision 1.10  98/09/15  18:45:21  da0g
// Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
// 
 * Revision 1.9  1998/06/02 10:39:40  reids
 * Increased overall efficiency of TCM.
 *
// Revision 1.8  98/05/30  12:00:37  reids
// Fixed "waitFor" if timepoint is achieved but there are still events on
//   the timer queue.
// Clear timeouts related to nodes that are completely achieved.
// 
// Revision 1.7  98/04/21  12:46:05  reids
// Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
//   particular event occurs.
// Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
//   a node by waiting some msecs after a particular event has occurred.
// 
// Revision 1.6  97/12/30  13:38:40  reids
// When node is terminated, signal (and remove) any timed-event in the timer
//   queue that is for that node, regardless of when it is supposed to fire.
// 
// Revision 1.5  97/12/30  12:26:51  reids
// Added flag to indicate whether node raised an exception (ie, failed).
// 
// Revision 1.4  97/12/29  17:06:27  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.3  97/12/22  16:52:58  reids
// Basically, added "data" field for CALLBACK_ACTION,
//  and started using "nodeData" for the activation data associated
//  with monitors, and the failure data associated with exceptions.
// 
// Revision 1.2  97/12/04  17:50:18  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:36  reids
// First release of TCM -- seems to be a stable version
// 
 *
 **************************************************************************/

#include "tcmBasics.h"
#include "tcmLogging.h"
#include "tcmGlobal.h"
#include "event.h"
#include "action.h"
#include "taskTree.h"
#include "agenda.h"
#include "exception.h"

/**************************************************************************
 *
 * Global Variables
 *
 **************************************************************************/

int Task_Tree_Node::globalId = 0;

Task_Tree_Node_List Task_Tree_Node::memory;

/**************************************************************************
 *
 * CLASS: Task_Tree_Node
 *
 **************************************************************************/

INSTANTIATE_REF_COUNT_FUNCTIONS(Task_Tree_Node);
INSTANTIATE_REF_COUNT_FUNCTIONS(Ref_Count);

/**************************************************************************
 *
 * FUNCTION: BOOLEAN isCurrentState(State_Enum state)
 *
 * DESCRIPTION: Return TRUE if the current state of the node is at
 *              the given state.
 *
 **************************************************************************/

BOOLEAN Task_Tree_Node::isCurrentState(State_Enum state) const
{ 
  return currentState.compare(state) == 0;
}

/**************************************************************************
 *
 * FUNCTION: BOOLEAN validState(State_Enum state)
 *
 * DESCRIPTION: Return TRUE if the current state of the node is at, or past, 
 *              the given state.
 *
 **************************************************************************/

BOOLEAN Task_Tree_Node::validState(State_Enum state) const
{ 
  return currentState.compare(state) <= 0;
}

/**************************************************************************
 *
 * FUNCTION: BOOLEAN noneExpected(Signal_Enum signal)
 *
 * DESCRIPTION: Return TRUE if the node does not expect to be receiving
 *              any more signals of the given type.
 *
 **************************************************************************/

/*inline*/ BOOLEAN Task_Tree_Node::_noneExpected(Signal_Enum signal) const
{ 
  return expectedEvents.count(signal) == 0;
}

/**************************************************************************
 *
 * FUNCTION: void addExpectedEvent(Signal_Enum signal, 
 *                                 Task_Tree_Ref const &signallingNode)
 *
 * DESCRIPTION: Add the <signal, signallingNode> event to the list of
 *              expected events.
 *
 **************************************************************************/

void Task_Tree_Node::addExpectedEvent(Signal_Enum signal, 
				      Task_Tree_Ref const &signallingNode,
				      const void *timerData)
{
  Event event(signal, signallingNode);

  event.setData(timerData);

  // Don't bother adding the signal if it is a termination signal and the
  //   node is already completed.
  // In addition, need to delete the associated requested event (it's done
  //   in this roundabout way in order to support distributed task trees).
  if (signal == Start_Terminating_Signal && isCurrentState(Achieved_State)) {
    removeRequestedEvent(event);
  } else {
    expectedEvents.insert(event);
  }
}

/**************************************************************************
 *
 * FUNCTION: void addRequestedEvent(State_Enum state, Signal_Enum signal,
 *                                  Task_Tree_Ref const &signalledNode)
 *
 * DESCRIPTION: Add the <signal, signalledNode, state> event to the list of 
 *              requested "state events"
 *
 * NOTE: Send the signal immediately if the node is already in (or past)
 *       the state.
 *
 **************************************************************************/

void Task_Tree_Node::addRequestedEvent(State_Enum state, Signal_Enum signal,
				       Task_Tree_Ref const &signalledNode,
				       void const *data)
{
  State_Event event(signal, signalledNode, state, data);

  if (validState(state)) {
    event.sendSignal(this);
  } else {
    requestedEvents.insert(event);
  }
}

/**************************************************************************
 *
 * FUNCTION: void addRequestedEvent (State_Enum state,
 *			             SIMPLE_CALLBACK_FN_TYPE callbackFn,
 *				     const void *data)
 *
 * DESCRIPTION: When this node transitions to "state", invoke
 *              the "callbackFn" on the "data" (actually, queue it first).
 *
 **************************************************************************/

void Task_Tree_Node::addRequestedEvent(State_Enum state, 
				       SIMPLE_CALLBACK_FN_TYPE callbackFn,
				       const void *data)
{
  State_Event event(callbackFn, data, state);

  if (validState(state)) {
    event.sendSignal(this);
  } else {
    requestedEvents.insert(event);
  }
}

/**************************************************************************
 *
 * FUNCTION: void removeExpectedEvent(Signal_Enum signal,
 *                                    Task_Tree_Ref const &signallingNode)
 *
 * DESCRIPTION: Remove (one of) the expected events that match
 *              the given signal and signalledNode
 *
 **************************************************************************/

void Task_Tree_Node::removeExpectedEvent(Signal_Enum signal,
					 Task_Tree_Ref const &signallingNode)
{
  if (signal == Timeout_Signal) {
    GET_TCM_GLOBAL(agenda).removeTimer(Timed_Event(Timeout_Signal, this, 0));
  } else {
    expectedEvents.removeItem(Event(signal, signallingNode));
  }
}

/**************************************************************************
 *
 * FUNCTION: void removeRequestedEvent(Signal_Enum signal,
 *                                     Task_Tree_Ref const &signalledNode)
 *
 * DESCRIPTION: Remove (one of) the <signal, signalledNode, state> requested
 *              events that match the given signal and signalledNode
 *
 **************************************************************************/

void Task_Tree_Node::removeRequestedEvent(Signal_Enum signal,
					  Task_Tree_Ref const &signalledNode)
{
  State_Event_Iterator listIter(&requestedEvents);
  State_Event *event;

  for (listIter.reset(); listIter; listIter++) {
    event = listIter();
    // This is kludgy, but needed to fix a bug that David Apfelbaum found
    // dealing with terminated nodes that have "afterSignal" constraints that
    // have not yet fired.
    if (event->getCallback() == afterSignalFn
	? (signal == ((AFTER_CONSTRAINT_PTR)event->getData())->signal &&
	   signalledNode == ((AFTER_CONSTRAINT_PTR)event->getData())->node)
	: (signal == event->getSignal() &&
	   signalledNode == event->getNode())) {
      if (event->getCallback() == afterSignalFn) {
	delete ((AFTER_CONSTRAINT_PTR)event->getData());
      }
      listIter.removeCurrent();
      return;
    }
  }
  // If we get here, the event was not found.
  // NOTE: There is a race condition that happens regularly when a child node is
  // terminated at the same time the parent is achieved (and "cleans up").
  // The race is that the Done_Terminating_Signal is in the event queue, so
  // that is why it does not appear in the event list.  In these cases, ignore
  // the warning.
  if (signal != Done_Terminating_Signal)
    tcmWarning("removeRequestedEvent: %s not found for %s in %s\n",
	       signalName(signal), signalledNode->instanceName(), instanceName());
}

static void removeTimedEvent (const Task_Tree_Node_Ptr node,
			      Signal_Enum signal, const void *data)
{
#ifdef DISTRIBUTED
  if (data != NULL) {
    // The event is queued in a different process, where the event data
    //   indicates which process.
    node->distributedRemoveTimer(signal, (STRING)data);
    return;
  }
#endif
  Timed_Event timedEvent(signal, node, 0);
  GET_TCM_GLOBAL(agenda).removeTimer(timedEvent, node);
}



/**************************************************************************
 *
 * FUNCTION: void removeRequestedEvent(Event event)
 *
 * DESCRIPTION: Remove either the expected event that matches the given 
 *              event or the timer event that matches the given event.
 *
 **************************************************************************/

void Task_Tree_Node::removeRequestedEvent(const Event &event)
{
  if (isTimerNode(event.getNode())) {
    removeTimedEvent(this, event.getSignal(), event.getData());
  } else {
    event.getNode()->removeRequestedEvent(event.getSignal(), this);
  }
}

/**************************************************************************
 *
 * FUNCTION: void taskTreeInsert (Task_Tree_Node_Ptr parent,
 *                                Task_Tree_Node_Ptr child,
 *		                  BOOLEAN isLastChild);
 *
 * DESCRIPTION: Add the child node to the parent.  If "isLastChild"
 *              is TRUE, treat the child node as a "last child"
 *              (I think it is always true in the way TCM was implemented!)
 *
 ****************************************************************/

void taskTreeInsert (Task_Tree_Node_Ptr parent, Task_Tree_Node_Ptr child,
		     BOOLEAN isLastChild)
{
  child->addParent(parent, isLastChild);
  parent->addChild(child);
  GET_TCM_GLOBAL(loggingFns).logChild(parent, child);
}

/**************************************************************************
 *
 * FUNCTION: void addEventPair (Task_Tree_Ref const &signallingNode, 
 *				State_Enum state, Signal_Enum signal,
 *				Task_Tree_Ref const &signalledNode)
 *
 * DESCRIPTION: Combines "addRequestedEvent" and "addExpectedEvent" in the
 *              obvious way.  Assures that a node being signalled will be
 *              expecting the event.
 *
 **************************************************************************/

void addEventPair (Task_Tree_Ref const &signallingNode, State_Enum state,
		   Signal_Enum signal, Task_Tree_Ref const &signalledNode,
		   void const *data)
{
  signallingNode->addRequestedEvent(state, signal, signalledNode, data);
  signalledNode->addExpectedEvent(signal, signallingNode);
}

/**************************************************************************
 *
 * FUNCTION: void addParent(Task_Tree_Ref const &parentNode, 
 *                          BOOLEAN canBeLastChild)
 *
 * DESCRIPTION: Add the parent node as the parent of "this" node.
 *              if "canBeLastChild" is TRUE, the node can be considered the
 *              "last child" of the parent.
 *
 **************************************************************************/
void Task_Tree_Node::addParent(Task_Tree_Ref const &parentNode,
			       BOOLEAN canBeLastChild, BOOLEAN addConstraints)
{
  parent = parentNode;
  canBeLast = canBeLastChild;

  if (addConstraints) {
    // Add the necessary "temporal constraints".  
    // Do just one side of this -- Have to assume a matching "addChild" is done

    // Start(Planning(parent)) <= Start(Planning(child))
    addExpectedEvent(Start_Planning_Signal, parent);
    // End(Planning(child)) <= End(Planning(parent))
    addRequestedEvent(Planned_State, Done_Planning_Signal, parent);

    // Start(Achieving(parent)) <= Start(Achieving(child))
    addExpectedEvent(Start_Achieving_Signal, parent);
    // End(Achieving(child)) <= End(Achieving(parent))
    addRequestedEvent(Achieved_State, Done_Achieving_Signal, parent);

    // Don't add termination constraints if parent is Root Node.
    if (!parentNode->isRootNode()) {
      // Start(Terminating(parent)) <= Start(Terminating(child))
      addExpectedEvent(Start_Terminating_Signal, parent);
      // End(Terminating(child)) <= End(Terminating(parent))
      addRequestedEvent(Terminated_State, Done_Terminating_Signal, parent);
    }
  }
}

/**************************************************************************
 *
 * FUNCTION: void addChild(Task_Tree_Ref const &childNode)
 *
 * DESCRIPTION: Add the child node as a child of "this" node.
 *
 **************************************************************************/
void Task_Tree_Node::addChild(Task_Tree_Ref const &childNode, BOOLEAN addConstraints)
{
  children.insertLast(childNode);

  if (isSuspended()) {
    childNode->suspend();
  }

  if (addConstraints) {
    // Add the necessary "temporal constraints".  
    // Do just one side of this -- Have to assume a matching "addParent" is done

    // Start(Planning(parent)) <= Start(Planning(child))
    addRequestedEvent(Planning_State, Start_Planning_Signal, childNode);
    // End(Planning(child)) <= End(Planning(parent))
    addExpectedEvent(Done_Planning_Signal, childNode);

    // Start(Achieving(parent)) <= Start(Achieving(child))
    addRequestedEvent(Achieving_State, Start_Achieving_Signal, childNode);
    // End(Achieving(child)) <= End(Achieving(parent))
    addExpectedEvent(Done_Achieving_Signal, childNode);

    // Don't add termination constraints if this is Root Node.
    if (!isRootNode()) {
      // Start(Terminating(parent)) <= Start(Terminating(child))
      addRequestedEvent(Terminating_State, Start_Terminating_Signal, childNode);
      // End(Terminating(child)) <= End(Terminating(parent))
      addExpectedEvent(Done_Terminating_Signal, childNode);
    }
  }
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr firstChild(void)
 *
 * DESCRIPTION: Return the first child of the node.
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::firstChild (void) const
{
  Const_Task_Tree_Node_Iterator listIter(children);

  return (listIter.getCurrent() != NULL ? **listIter() : NULL);
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr lastChild(BOOLEAN ignoreVeryLast)
 *
 * DESCRIPTION: Find the "last child" (the last one on the children list
 *              that has "canBeLast" set).  Don't count the very last if
 *              "ignoreVeryLast" is TRUE.
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::lastChild(BOOLEAN ignoreVeryLast) const
{
  Const_Task_Tree_Node_Iterator listIter(children);

  listIter.resetLast();
  if (listIter) {
    if (ignoreVeryLast) listIter--;
    for (; listIter; listIter--) {
      if ((*listIter())->canBeLast) {
	return **listIter();
      }
    }
  }
  return NULL;
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr previousChild(Task_Tree_Ref const &child);
 *
 * DESCRIPTION: Find the child in the list before the given child node
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::previousChild(Task_Tree_Ref const &child) const
{
  Const_Task_Tree_Node_Iterator listIter(children);
  Task_Tree_Node_Ptr current, last = NULL;

  for (listIter.reset(); listIter; listIter++) {
    current = **listIter();
    if (current == *child) {
      return last;
    }
    last = current;
  }
  tcmError("previousChild: Node %s not a child of %s\n",
	   instanceName(), child->instanceName());
  return NULL;
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr nextChild(Task_Tree_Ref const &child);
 *
 * DESCRIPTION: Find the child in the list before the given child node
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::nextChild(Task_Tree_Ref const &child) const
{
  Const_Task_Tree_Node_Iterator listIter(children);

  for (listIter.reset(); listIter; listIter++) {
    if (*listIter() == *child) {
      listIter++;
      return (listIter != NULL ? **listIter() : NULL);
    }
  }
  tcmError("nextChild: Node %s not a child of %s\n",
	   instanceName(), child->instanceName());
  return NULL;
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr childNamed(STRING const name)
 *
 * DESCRIPTION: Find the (first) child of the node that has the given
 *              name, if any.
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::childNamed(STRING const name) const
{
  Const_Task_Tree_Node_Iterator listIter(children);

  for (listIter.reset(); listIter; listIter++) {
    if (streq((*listIter())->instanceName(), name)) {
      return **listIter();
    }
  }
  return NULL;
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr ancestorNamed(STRING const name)
 *
 * DESCRIPTION: Find the first ancestor of the node (parent, grandparent, etc)
 *              that has the given name, if any.
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::ancestorNamed(STRING const name) const
{
  Task_Tree_Ref parentRef = parent;
	
  while (validRef(parentRef)) {
    if (streq(parentRef->instanceName(), name))
      return *parentRef;
    else
      parentRef = parentRef->parent;
  }
  return NULL;
}

/**************************************************************************
 *
 * FUNCTION: void signal(const Event &event)
 *
 * DESCRIPTION: If event is in the expectedEvents of the node, delete it
 *              from the list and invoke the signal function assocated
 *              with the signal of the event.
 *
 **************************************************************************/

void Task_Tree_Node::signal(const Event &event)
{
  Signal_Enum signal = event.getSignal();

  if (expectedEvents.member(event)) {
#ifdef TRACE_SIGNALS
    tcmMessage("%s handling signal %s from %s\n", instanceName(),
	       signalName(signal), event.getNode()->instanceName());
#endif

    if (signal == Activate_Signal)
      setNodeData(event.getData());

    // There may be multiple such events -- just remove one.
    expectedEvents.removeItem(event);
    _processSignal(signal);
  } else {
    // NOTE: There is a race condition that happens regularly when a child node
    // is terminated at the same time the parent is achieved (and "cleans up").
    // The race is that the Done_Terminating_Signal is in the event queue, so
    // that is why it does not appear in the event list.  In these cases, ignore
    // the warning.
    if (signal != Done_Terminating_Signal)
      tcmWarning("%s not expecting %s signal from %s\n", instanceName(), 
		 signalName(signal), event.getNode()->instanceName());
  }
}

/**************************************************************************
 *
 * FUNCTION: void processRequestedEvents(void)
 *
 * DESCRIPTION: Go through the requestedEvents; If the currentState matches
 *              the state in one of the State_Event requests, then delete it
 *              from the list and signal the event.
 *
 **************************************************************************/

void Task_Tree_Node::processRequestedEvents(void)
{
  State_Event_Iterator listIter(requestedEvents);
  State_Enum  signalState;
  Signal_Enum eventSignal;

  for (listIter.reset(); listIter; listIter++) {
    signalState = listIter()->getState();
    if (this->validState(signalState)) {
      // State is current (or has past) -- send the signal.
      // Except: Don't terminate unecessarily, that is:
      //         Don't send the signal if it is start terminating,
      //         and the node is in the terminating/terminated state,
      //         and the signal state is not terminating/terminated.
      eventSignal = listIter()->getSignal();
      if (addEventP(eventSignal, signalState)) {
	listIter()->sendSignal(this);
      } else {
	listIter()->getNode()->removeExpectedEvent(eventSignal, this);
#ifdef TRACE_AFTER_SIGNAL
	tcmWarning("PRE: %s %s <%s, %s, %s>\n", this->instanceName(),
		   stateName(this->theCurrentState(Alive_State)),
		   signalName(eventSignal),
		   listIter()->getNode()->instanceName(),
		   stateName(signalState));
#endif
      }
      listIter.removeCurrent();
    }
  }
}

#include "commInterface.h"

static void cleanUpTask (const void *dummy, const Task_Tree_Ref &node)
{
  if (node->isCurrentState(Alive_State)) {
#ifdef TRACE_DELETIONS
#ifdef TRACE_UBER
    tcmMessage("Cleaning up %s {%d}\n", node->instanceName(), node->getNodeId());
#else
    tcmMessage("Cleaning up %s\n", node->instanceName());
#endif
#endif
    Task_Tree_Ref parent = node->getParent();
    if (parent != NULL && !parent->isRootNode()) {
      node->removeRequestedEvent(Done_Terminating_Signal, parent);
      parent->removeExpectedEvent(Done_Terminating_Signal, node);
    }

    node->cleanUp();
  } 
#ifdef TRACE_DELETIONS
  else
    tcmMessage("Skipping %s\n", node->instanceName());
#endif
}

/**************************************************************************
 *
 * FUNCTION: void transitionTo(State_Enum newState)
 *
 * DESCRIPTION: Check that the transition is legal.
 *              If so, run the state transition function for the newState.
 *              If the function returns TRUE, then update the state to the new
 *              state and process the requested events.
 *
 **************************************************************************/
void Task_Tree_Node::transitionTo(State_Enum newState)
{
  State_Enum nextState;
  
  if (newState == Unknown_State) {
    tcmError("Trying to transition to Unknown_State");
  } else if (validState(newState)) {
    tcmWarning("%s trying to transition from %s to %s\n", instanceName(),
	       stateName(theCurrentState(newState)), stateName(newState));
  } else {
    nextState = currentState.next(theCurrentState(newState));
    if (nextState != newState) {
      // Ignore multi-state transitions
#ifdef TRACE_TRANSITIONS
      tcmMessage("%s requested multi-state transition (from %s to %s)\n", 
		 instanceName(), stateName(theCurrentState(newState)),
		 stateName(newState));
#endif
      // _processSignal(stateToSignal(nextState));
    } else if (_handleTransition(newState)) {
      GET_TCM_GLOBAL(loggingFns).logStateChange(this, newState);
      currentState.set(newState);
      processRequestedEvents();
      // Re-enable termination of nodes after they have been achieved
      if (isTerminationDelayed() && isCurrentState(Achieved_State)) {
	BOOLEAN terminateP = (_delayedTermination == Terminate);
	enableTermination();
	// Terminate "delayed termination" nodes, if necessary
	if (terminateP) {
#ifdef TRACE_TRANSITIONS
	  tcmMessage("Terminating delayed node %s\n", instanceName());
#endif
	  terminate();
	}
      }
      
      nextState = currentState.next(newState);
      if (nextState != Unknown_State) {
	// Keep trying to transition
	_processSignal(stateToSignal(nextState));
      }
    }
  }
}

/**************************************************************************
 *
 * FUNCTION: void doneHandling(void)
 *
 * DESCRIPTION: Node's action just completed -- signal a "success" event.
 *
 **************************************************************************/
void Task_Tree_Node::doneHandling (void)
{
  Event doneEvent(Done_Handling_Signal, this);
  
  _doneHandling = TRUE;
  _postponed = FALSE;
  doneEvent.sendSignal(this);
}

/****************************************************************
 *
 * FUNCTION: BOOLEAN getTreeFailed(void)
 *
 * DESCRIPTION: Return TRUE if any node of the tree failed, FALSE otherwise
 *
 *****************************************************************/
BOOLEAN Task_Tree_Node::getTreeFailed (void) const
{
  if (_failed) {
    // This node failed
    return FALSE;
  } else {
    // Check its children
    Const_Task_Tree_Node_Iterator listIter(children);

    for (listIter.reset(); listIter; listIter++) {
      if ((*listIter())->getTreeFailed()) return FALSE;
    }
    // If we get here, none of the children, or their descendants, have failed
    return TRUE;
  }
}

/**************************************************************************
 *
 * FUNCTION: BOOLEAN terminate(void)
 *
 * DESCRIPTION: Terminate the task tree node, and all its descendants.
 *              Return TRUE if successful.
 *
 **************************************************************************/
BOOLEAN Task_Tree_Node::terminate (void)
{
  if (isRootNode()) {
    tcmError("Cannot terminate the root node\n");
    return FALSE;
  } else if (validState(Terminating_State)) {
    tcmWarning("Trying to terminate already dead node %s\n", instanceName());
    return FALSE;
  } else {
    // Force monitor to be handled, so that it terminates correctly
    if (isMonitor() && !validState(Handled_State)) doneHandling();

    transitionTo(Terminating_State);
    return TRUE;
  }
}

/**************************************************************************
 *
 * FUNCTION: BOOLEAN deallocate(void)
 *
 * DESCRIPTION: Destroy the task tree node.  Only works if node is not
 *              connected to task tree, and has no children (otherwise
 *              use "terminate").  Return TRUE if successful.
 *
 **************************************************************************/

BOOLEAN Task_Tree_Node::deallocate (void)
{
  if (isRootNode()) {
    tcmError("Cannot deallocate the root node\n");
    return FALSE;
  } else if (validRef(parent)) {
    tcmWarning("Trying to deallocate node %s, which is already connected to "
	       "the task tree\n", instanceName());
    return FALSE;
  } else if (children.size() > 0) {
    tcmWarning("Trying to deallocate node %s, which already has children\n",
	       instanceName());
    return FALSE;
  } else {
    transitionTo(Terminating_State);
    // Since the node is not yet inserted in the task tree, need to force
    // it to transition.
    _processSignal(Start_Achieving_Signal);
    if (isGoal()) _processSignal(Start_Planning_Signal);
    return TRUE;
  }
}

/**************************************************************************
 *
 * FUNCTION: void suspend(void)
 *
 * DESCRIPTION: Suspend the task tree node, and all its descendants.
 *              Suspended nodes cannot transition from Unhandled -> Handling,
 *              which means that their actions cannot be invoked.  However,
 *              other transitions (such as Alive -> Killing) *can* take place
 *              even when a node is suspended.
 *
 * NOTES: Need to recurse even if the node itself is currently suspended,
 *        because not all of its descendants may currently be suspended 
 *        (eg, some may have subsequently been unsuspended).
 *
 **************************************************************************/
void Task_Tree_Node::suspend (void)
{
  Task_Tree_Node_Iterator listIter(children);

  if (!_suspended) {
    _suspended = TRUE;
    GET_TCM_GLOBAL(loggingFns).logSuspension(this, TRUE);

    // Prevent suspended node from starting to be handled
    if (isCurrentState(Unhandled_State)) {
      addExpectedEvent(Start_Handling_Signal, this);
    }
  }

  for (listIter.reset(); listIter; listIter++) {
    (*listIter())->suspend();
  }
}

/**************************************************************************
 *
 * FUNCTION: void unsuspend(void)
 *
 * DESCRIPTION: Unsuspend the task tree node, and all its descendants.
 *              Queue any node actions that might be enabled.
 *
 * NOTES: Need to recurse even if the node itself is currently unsuspended,
 *        because some of its descendants may currently be suspended.
 *
 **************************************************************************/
void Task_Tree_Node::unsuspend (void)
{
  Task_Tree_Node_Iterator listIter(children);

  if (_suspended) {
    GET_TCM_GLOBAL(loggingFns).logSuspension(this, FALSE);
    _unsuspendNode();
  }

  for (listIter.reset(); listIter; listIter++) {
    (*listIter())->unsuspend();
  }
}

/**************************************************************************
 *
 * FUNCTION: void _initialize(void)
 *
 * DESCRIPTION: Initialize task tree node data structure
 *
 **************************************************************************/
void Task_Tree_Node::_initialize(STRING nodeTypeName)
{
  id = globalId++;
  parent = NULL;

  _nodeTypeName = strdup(nodeTypeName);
    // Default is instanceName == nodeTypeName; Change it using setInstanceName
  name = strdup(_nodeTypeName);
    /* Virtual Nodes (Distributed Tasks) need to      *
     * distinguish between identically named nodes... */
  _overloadedNodeTypeNameIndex = DEFAULT_OVERLOADED_TASK_NAME_INDEX;

  _action = NULL;
  _nodeData = NULL;
  _userData = NULL;
  canBeLast = FALSE;
  _persistent = FALSE;
  _suspended = FALSE;
  _doneHandling = FALSE;
  _postponed = FALSE;
  _failed = FALSE;
  _delayedTermination = Nominal;
#ifdef DISTRIBUTED
  _vaddress.creator = _vaddress.host = NULL;
  _vaddress.id = 0;
#endif

	/* Threading support. */
  isThreaded = FALSE;

  // This is the event "success" and "failure" generate to signal end handling
  addExpectedEvent(Done_Handling_Signal, this);
}

/**************************************************************************
 *
 * FUNCTION: void _intraNodeConstraints(void)
 *
 * DESCRIPTION: Set the needed intra-node constraints for task tree nodes.
 *
 * NOTES: Encodes the following constraints:
 *        End(Handling(node)) <= End(Achieving(node))
 *        End(Handling(node)) <= End(Planning(node))
 *
 ************************************************************************/

void Task_Tree_Node::_intraNodeConstraints (void)
{
#ifdef USE_EQUALITIES
  // End(Handling(node)) <= End(Achieving(node))
  addEventPair(this, Handled_State, Done_Achieving_Signal, this);
  
  // End(Handling(node)) <= End(Planning(node))
  addEventPair(this, Handled_State, Done_Planning_Signal, this);
#endif
}

/**************************************************************************
 *
 * FUNCTION: void _handleSignal(Signal_Enum signal)
 *
 * DESCRIPTION: A signal just arrived at the node; Handle it by transitioning
 *              to the associated state if no other events of the given signal
 *              type are expected.
 *
 **************************************************************************/
void Task_Tree_Node::_handleSignal(Signal_Enum signal)
{
#ifdef USE_EQUALITIES
  State_Enum newState = signalToState(signal);
  if (newState == Unknown_State) {
    tcmError("_handleSignal: Cannot handle signal %d\n", signal);      
  } else {
    transitionTo(newState);
  }
#else				// !USE_EQUALITIES
  switch (signal) {
  case Start_Handling_Signal: 
    transitionTo(Handling_State); break;
  case Done_Handling_Signal: 
    if (_noneExpected(Done_Achieving_Signal)) transitionTo(Achieved_State);
    if (_noneExpected(Done_Planning_Signal)) transitionTo(Planned_State);
    transitionTo(Handled_State);
    break;
    
  case Start_Planning_Signal: transitionTo(Planning_State); break;
  case Done_Planning_Signal:
    if (validState(Handled_State)) {
      transitionTo(Planned_State);
      if (_noneExpected(Done_Achieving_Signal)) transitionTo(Achieved_State);
    }
    break;
    
  case Start_Achieving_Signal: transitionTo(Achieving_State); break;
  case Done_Achieving_Signal:
    if (validState(Handled_State) && validState(Planned_State))
      transitionTo(Achieved_State);
    break;
    
  case Start_Terminating_Signal: 
    // Since "Start_Terminating" is an immediate signal, may already have
    // made this transition
    if (!validState(Terminating_State)) transitionTo(Terminating_State);
    break;
  case Done_Terminating_Signal:
    // Only do this if the node is in the process of terminating
    if (validState(Terminating_State)) transitionTo(Terminated_State);
    break;
    
  default:
    tcmError("_handleSignal: Cannot handle signal %d\n", signal);
  }
#endif				// !USE_EQUALITIES
}

/**************************************************************************
 *
 * FUNCTION: void _handleTransition (State_Enum newState)
 *
 * DESCRIPTION: The node wants to transition to the newState; Do it.
 *              Different classes of nodes may transition differently.
 *
 **************************************************************************/
BOOLEAN Task_Tree_Node::_handleTransition(State_Enum newState)
{
  BOOLEAN retVal;

  switch (newState) {
  case Unhandled_State: retVal = TRUE; break;
  case Handling_State: 
    retVal = !_suspended;
    if (retVal) GET_TCM_GLOBAL(agenda).queueTask(this);
    break;
  case Handled_State:  retVal = TRUE; break;
    
  case Unplanned_State: retVal = TRUE; break;
  case Planning_State:  retVal = TRUE; break;
  case Planned_State:   retVal = TRUE; break;
    
  case Unachieved_State: retVal = TRUE; break;
  case Achieving_State:  retVal = TRUE; break;
  case Achieved_State:   
    _forceTimeouts(); 
    if (!_persistent) {
      // Clean up, releasing memory
      // Queue it to make sure all other appropriate events are handled first
      Event(cleanUpTask, NULL).sendSignal(this);
      }
    retVal = TRUE; break;
    
  case Alive_State:   retVal = TRUE; break;
  case Terminating_State: 
    if (isTerminationDelayed()) {
#ifdef TRACE_TRANSITIONS
      tcmMessage("Termination delayed for %s\n", instanceName());
#endif
      _delayedTermination = Terminate;
      retVal = FALSE;
    } else {
      // Force postponed task to be handled, so that it terminates correctly
      if (isPostponed() && !validState(Handled_State)) doneHandling();
      _startTerminating(); retVal = TRUE;
    }
    break;
    // Don't delete right away, since other code needs to access "this" node
  case Terminated_State:   _destroy(); retVal = TRUE; break;
    
  default:
    tcmError("_handleTransition: Cannot handle state %d\n", newState);
    retVal = FALSE;
  }

#ifdef TRACE_TRANSITIONS
  if (retVal) {
    tcmMessage("%s transitioning from %s to %s\n",
	       instanceName(), stateName(theCurrentState(newState)),
	       stateName(newState));
  }
#endif
  return retVal;
}

/**************************************************************************
 *
 * FUNCTION: void _startTerminating (void)
 *
 * DESCRIPTION: Start the process of terminating this node:
 *
 * NOTES: Add events to prevent node from transitioning to "Terminated" until 
 *        it is Handled, Planned and Achieved (do it here for efficiency -- 
 *        since terminating is relatively rare, don't want to have to handle  
 *        these signals all the time.
 *        Remove (and signal) any pending timeout signals for this node.
 *        Add any termination actions as goal nodes.
 **************************************************************************/

void Task_Tree_Node::_startTerminating (void)
{
  Task_Tree_Node_Iterator termIter(terminationRefs);

  if (!validState(Handled_State)) {
    addEventPair(this, Handled_State, Done_Terminating_Signal, this);
  }
  if (isCurrentState(Planning_State)) {
    addEventPair(this, Planned_State, Done_Terminating_Signal, this);
  }
  if (!validState(Achieved_State)) {
    addEventPair(this, Achieved_State, Done_Terminating_Signal, this);
  }
  if (_suspended) _unsuspendNode();

  _forceTimeouts();

  // Run the termination actions, unless the node has not yet 
  //   been handled, or is already achieved.
  // NOTE: A node can be in the "handling" state but the code not
  //   yet run, if it is on the task queue
#if 0
  bool doOnTerminate = (terminationRefs.size() > 0 &&
			validState(Handling_State) &&
			!validState(Achieved_State)
			&& !GET_TCM_GLOBAL(agenda).taskQueued(this));
  for (termIter.reset(); doOnTerminate && termIter; termIter++) {
    if (doOnTerminate) {
      (**termIter())->delayTermination();
      taskTreeInsert(this, **termIter(), TRUE);
    } else {
      // Don't need to run the "on terminate" nodes -- just clean them up
      (*termIter())->terminate();
    }
    termIter.removeCurrent();
  }
#else
  if (validState(Handling_State) && !validState(Achieved_State)
      && !GET_TCM_GLOBAL(agenda).taskQueued(this)) {
    for (termIter.reset(); termIter; termIter++) {
      (**termIter())->delayTermination();
      taskTreeInsert(this, **termIter(), TRUE);
      termIter.removeCurrent();
    }
  }
#endif
}

/**************************************************************************
 *
 * FUNCTION: void _destroy (void)
 *
 * DESCRIPTION: If the node is terminated, null out all the slots and remove
 *              from task tree -- node will actually be deleted when all
 *              references to it are deleted.
 *
 **************************************************************************/

#ifdef DISTRIBUTED
static void forgetVirtual1(const void *data)
{
  Task_Tree_Node_Ptr node = (Task_Tree_Node_Ptr)data;

  if (node->validVirtualAddress()) {
    node->forgetVirtual(node->getVirtualAddress());
  }
}
#endif

void printEventList(Event_List const &eventList);
void printStateEventList(State_Event_List const &eventList);

void Task_Tree_Node::_destroy (void)
{
  Event_Iterator expectIter(expectedEvents);
  Exception_Iterator exceptIter(exceptions);
  Task_Tree_Node_Iterator termIter(terminationRefs);
  Task_Tree_Node_Ptr currentNode;
  
  // I don't think the current list implementation will work right 
  // if "removeRequestedEvent" removes something from the list being
  // iterated on here.  This is wasteful, but should work all the time.
  for (expectIter.reset(); expectIter; 
       (currentNode == this ? expectIter.reset() : expectIter++)) {
    currentNode = *(expectIter()->getNode());
    removeRequestedEvent(*expectIter());
    expectIter.removeCurrent();
  }  
  // Might not have been added to the task tree, yet
  if (validRef(parent)) {
    parent->removeChild(this);
    parent = NULL;
  }
  for (exceptIter.reset(); exceptIter; exceptIter++) {
    exceptIter.removeCurrent();
  }

  // Kill any remaining "on termination" nodes, since they haven't been run
  for (termIter.reset(); termIter; termIter++) {
    (*termIter())->deallocate();
    termIter.removeCurrent();
  }

  // Force action to be clean (in case the action contains a Task_Tree_Ref
  //  referring to the task itself)
  _action = NULL;

#ifdef DISTRIBUTED
  // Queue it to make sure all other appropriate events are handled first
  Event(forgetVirtual1, this).sendSignal(this);
#endif

#ifdef TRACE_DELETIONS
  if (children.size() > 0)
    tcmWarning("%s: Terminated node with children\n", instanceName());
  if (expectedEvents.size() > 0) {
    tcmWarning("%s: Terminated node with expected events\n", instanceName());
    printEventList(expectedEvents);
  }
  if (requestedEvents.size() >
      requestedEvents.count(Done_Terminating_Signal)) {
    tcmWarning("%s: Terminated node with requested events\n", instanceName());
    printStateEventList(requestedEvents);
  }
#endif
#ifdef  USE_CACHED_LAST_CHILD
  _cachedLastChild = NULL;
#endif
}


/**************************************************************************
 *
 * FUNCTION: void _unsuspendNode(void)
 *
 * DESCRIPTION: Private function to unsuspend the task tree node.
 *              Queue any node actions that might be enabled.
 *
 **************************************************************************/
void Task_Tree_Node::_unsuspendNode (void)
{
  _suspended = FALSE;

  // Enable the suspended node to start being handled, if ready
  if (isCurrentState(Unhandled_State)) {
    Event event(Start_Handling_Signal, this);
    event.sendSignal(this);
  }
}

/**************************************************************************
 *
 * CLASS: Task_Tree_Goal_Node
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Goal_Node (STRING nodeName)
 *
 * DESCRIPTION: Constructor function for goal node class.
 *
 ************************************************************************/

Task_Tree_Goal_Node::Task_Tree_Goal_Node (STRING nodeName) 
  : Task_Tree_Node(nodeName)
{
  _intraNodeConstraints();
  GET_TCM_GLOBAL(loggingFns).logNode(this);
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Goal_Node (STRING nodeName, 
 *                                const TCM_Action_Ref &nodeAction)
 *
 * DESCRIPTION: Constructor function for goal node class.
 *
 ************************************************************************/

Task_Tree_Goal_Node::Task_Tree_Goal_Node (STRING nodeName,
					  const TCM_Action_Ref &nodeAction)
  : Task_Tree_Node(nodeName, nodeAction)
{
  _intraNodeConstraints();
  GET_TCM_GLOBAL(loggingFns).logNode(this);
  GET_TCM_GLOBAL(loggingFns).logActionData(this);
}

/**************************************************************************
 *
 * FUNCTION: void _intraNodeConstraints(void)
 *
 * DESCRIPTION: Set the needed intra-node constraints for goal nodes.
 *
 * NOTES: Encodes the following constraints:
 *        Start(Handling(node)) <= Start(Achieving(node))
 *        End(Handling(node)) <= End(Achieving(node))
 *        End(Handling(node)) <= End(Planning(node))
 *        Start(Handling(node)) == Start(Planning(node))
 *
 *   Until equalities are supported, it is best to handle these constraints
 *   explicitly in "_handleSignal" (o/w it is likely that cycles will arise).
 *
 ************************************************************************/

void Task_Tree_Goal_Node::_intraNodeConstraints (void)
{
#ifdef USE_EQUALITIES
  // Start(Handling(node)) <= Start(Achieving(node))
  addEventPair(this, Handling_State, Start_Achieving_Signal, this);
  
  // End(Handling(node)) <= End(Achieving(node))
  addEventPair(this, Handled_State, Done_Achieving_Signal, this);
  
  // End(Handling(node)) <= End(Planning(node))
  addEventPair(this, Handled_State, Done_Planning_Signal, this);
  
  // Start(Handling(node)) == Start(Planning(node))
  addEventPair(this, Handling_State, Start_Planning_Signal, this);
  addEventPair(this, Planning_State, Start_Handling_Signal, this);
#endif				// USE_EQUALITIES
}

/**************************************************************************
 *
 * FUNCTION: void _handleSignal(Signal_Enum signal)
 *
 * DESCRIPTION: A signal just arrived at the goal node; Handle it.
 *              Hands off to the Task_Tree_Node::_handleSignal function
 *              if the method of handling is the same (to avoid duplication,
 *              and to make commonalities apparent).
 *
 **************************************************************************/
void Task_Tree_Goal_Node::_handleSignal(Signal_Enum signal)
{
#ifdef USE_EQUALITIES
  Task_Tree_Node::_handleSignal(signal);
#else				// !USE_EQUALITIES
  switch (signal) {
  case Start_Handling_Signal:
    if (_noneExpected(Start_Achieving_Signal)) transitionTo(Achieving_State);
    if (_noneExpected(Start_Planning_Signal))  {
      transitionTo(Planning_State);
      transitionTo(Handling_State);
    }
    break;
    
  case Start_Planning_Signal: 
    if (_noneExpected(Start_Handling_Signal)) {
      transitionTo(Planning_State); 
      transitionTo(Handling_State);
    }
    break;
    
  case Start_Achieving_Signal:
    if (_noneExpected(Start_Handling_Signal)) transitionTo(Achieving_State);
    break;
    
  default:
    Task_Tree_Node::_handleSignal(signal); break;
  }
#endif				// !USE_EQUALITIES
}

/**************************************************************************
 *
 * CLASS: Task_Tree_Command_Node
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Command_Node (STRING nodeName)
 *
 * DESCRIPTION: Constructor function for command node class.
 *
 ************************************************************************/

Task_Tree_Command_Node::Task_Tree_Command_Node (STRING nodeName) 
  : Task_Tree_Node(nodeName)
{
  _intraNodeConstraints();
  GET_TCM_GLOBAL(loggingFns).logNode(this);
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Command_Node (STRING nodeName, 
 *                                   const TCM_Action_Ref &nodeAction)
 *
 * DESCRIPTION: Constructor function for command node class.
 *
 ************************************************************************/

Task_Tree_Command_Node::Task_Tree_Command_Node 
                        (STRING nodeName, const TCM_Action_Ref &nodeAction)
  : Task_Tree_Node(nodeName, nodeAction)
{
  _intraNodeConstraints();
  GET_TCM_GLOBAL(loggingFns).logNode(this);
  GET_TCM_GLOBAL(loggingFns).logActionData(this);
}

/**************************************************************************
 *
 * FUNCTION: void _intraNodeConstraints(void)
 *
 * DESCRIPTION: Set the needed intra-node constraints for command nodes.
 *
 * NOTES: Encodes the following constraints:
 *        Start(Handling(node)) == Start(Achieving(node))
 *        End(Handling(node)) <= End(Achieving(node))
 *
 *   Until equalities are supported, it is best to handle these constraints
 *   explicitly in "_handleSignal" (o/w it is likely that cycles will arise).
 *
 *   Also, need to explicitly transition to the "Planned" state, since 
 *   command nodes are already planned out, by definition (since they are 
 *   supposed to be leaf nodes).
 ************************************************************************/

void Task_Tree_Command_Node::_intraNodeConstraints (void)
{
#ifdef USE_EQUALITIES
  // Start(Handling(node)) == Start(Achieving(node))
  addEventPair(this, Handling_State, Start_Achieving_Signal, this);
  addEventPair(this, Achieving_State, Start_Handling_Signal, this);
  
  // End(Handling(node)) <= End(Achieving(node))
  addEventPair(this, Handled_State, Done_Achieving_Signal, this);
#endif				// USE_EQUALITIES
  
  transitionTo(Planning_State);
  transitionTo(Planned_State);
}

/**************************************************************************
 *
 * FUNCTION: void _handleSignal(Signal_Enum signal)
 *
 * DESCRIPTION: A signal just arrived at the command node; Handle it.
 *
 **************************************************************************/
void Task_Tree_Command_Node::_handleSignal(Signal_Enum signal)
{
#ifdef USE_EQUALITIES
  Task_Tree_Node::_handleSignal(signal);
#else				// !USE_EQUALITIES
  switch (signal) {
  case Start_Handling_Signal:
    if (_noneExpected(Start_Achieving_Signal)) {
      transitionTo(Achieving_State);
      transitionTo(Handling_State);
    }
    break;
    
  case Done_Handling_Signal: 
    if (_noneExpected(Done_Achieving_Signal)) transitionTo(Achieved_State);
    transitionTo(Handled_State);
    break;
    
  case Start_Planning_Signal: break;
  case Done_Planning_Signal:  break;
    
  case Start_Achieving_Signal:
    if (_noneExpected(Start_Handling_Signal)) {
      transitionTo(Achieving_State);
      transitionTo(Handling_State);
    }
    break;
    
  default:
    Task_Tree_Node::_handleSignal(signal); break;
  }
#endif				// !USE_EQUALITIES
}

/**************************************************************************
 *
 * FUNCTION: void displayTree (int position)
 *
 * DESCRIPTION: Display the node name/status, and those of its children.
 *
 **************************************************************************/

// Probably a more efficient way to do this, but not called very often, so not critical
inline void pr_space (FILE *stream, int n) { for (int i=n; i>0; --i) fprintf(stream, " "); }

const int displayIncrement = 3;

static char blank_line[] = "                                                 ";

void Task_Tree_Node::displayTree (FILE *stream, int position) const
{
  //pr_space(stream, position);
  fprintf(stream, "%.*s%s {%d} %s\n", position, blank_line, 
	  instanceName(), id, _nodeStatusString());
  Const_Task_Tree_Node_Iterator childIter(children);
  for (; childIter; childIter++) {
    (*childIter())->displayTree(stream, position + displayIncrement);
  }
}

STRING Task_Tree_Node::_nodeStatusString (void) const
{
  static char statusString[15];
  
  snprintf(statusString, sizeof(statusString)-1, "[%s|%s|%s|%s]",
	   (isCurrentState(Unhandled_State)
	    ? "uh" : isCurrentState(Handled_State) ? "hd" : "hg"),
	   (isCurrentState(Unachieved_State)
	    ? "ua" : isCurrentState(Achieved_State) ? "ad" : "ag"),
	   (isCurrentState(Unplanned_State)
	    ? "up" : isCurrentState(Planned_State) ? "pd" : "pg"),
	   (isCurrentState(Alive_State)
	    ? "al" : isCurrentState(Terminated_State) ? "td" : "tg"));
  
  return statusString;
}

/**************************************************************************
 *
 * FUNCTION: void _forceTimeouts ()
 *
 * DESCRIPTION: Signal (and remove) any timed-event in the timer queue that
 *              is for this node, regardless of when it is supposed to fire
 *              (except if it is a "Timeout" signal, which is handled
 *              separately by the monitors).
 *
 * NOTE: Only need to do this for signals relating to the handling,
 *       planning or achievement phases.
 **************************************************************************/

void Task_Tree_Node::_forceTimeouts (void)
{
  Event_Iterator expectIter(expectedEvents);
  Event *event;
  Signal_Enum signal;

  for (expectIter.reset(); expectIter; expectIter++) {
    event = expectIter();
    if (isTimerNode(event->getNode())) {
      signal = event->getSignal();
      // "Timeout" signals are handled separately by the monitors.
      if (signal != Timeout_Signal) {
	// Send this signal only if related to handling/planning/achievement
	if (signal != Activate_Signal &&
	    stateToPhase(signalToState(signal)) != Terminating_Phase) {
	  Event timeoutEvent(signal, this);
	  timeoutEvent.sendSignal(event->getNode());
	} else {
	  expectIter.removeCurrent();
	}
#ifdef TRACE_FORCE
	tcmMessage("FORCE: %s %s\n", instanceName(), signalName(signal));
#endif
	removeTimedEvent(this, signal, event->getData());
      }
    }
  }
}

/**************************************************************************
 *
 * FUNCTION: void addTerminationAction(const TCM_Task_Tree_Ref &terminationRef)
 *
 * DESCRIPTION: Add this node as a child if the "this" node is ever terminated.
 *
 **************************************************************************/

void
Task_Tree_Node::addTerminationAction (const TCM_Task_Tree_Ref &terminationRef)
{
  terminationRefs.insertLast(terminationRef);
}

/* The parents of persistent nodes are persistent themselves.
 * Currently, does not work right if node goes from non-persistent to
 * persistent -- need to check all the children
 */
void Task_Tree_Node::setPersistence (BOOLEAN value)
{ 
  _persistent = value;
  if (value == TRUE) {
    if (parent != NULL) {
      parent->setPersistence(value);
    }
  }
}

/* Need a special function for determining whether it is the root node
 * in order to work correctly for virtual nodes 
 */
BOOLEAN Task_Tree_Node::isRootNode  (void) const
{ 
  return GET_TCM_GLOBAL(rootNode) == this;
}

/**************************************************************************
 *
 * Functions to map between a task tree node and its id number
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: int remember(void)
 *
 * DESCRIPTION: Memoize the node, so that it can be recalled
 *
 **************************************************************************/
int Task_Tree_Node::remember (void)
{
  memory.insertLast(this);
  return this->id;
}

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Node_Ptr recall (int refID)
 *
 * DESCRIPTION: Find the task tree node associated with the give id number
 *
 **************************************************************************/
Task_Tree_Node_Ptr Task_Tree_Node::recall (int refID) const
{
  Const_Task_Tree_Node_Iterator nodeIter(memory);
	
  for (; nodeIter; nodeIter++) {
    if (refID == (*nodeIter())->id) {
      return **nodeIter();
    }
  }
  return NULL;
}

/**************************************************************************
 *
 * FUNCTION: void forget (int refID)
 *
 * DESCRIPTION: Un-memoize the node, freeing up storage
 *
 **************************************************************************/
void Task_Tree_Node::forget (int refID)
{
  Task_Tree_Node_Iterator nodeIter(memory);
	
  for (; nodeIter; nodeIter++) {
    if (refID == (*nodeIter())->id) {
      nodeIter.removeCurrent();
      break;
    }
  }
}

#ifdef DISTRIBUTED
/* 
 * Put here so that do not need to link with virtual.cc
 *   unless one really needs the distributed functionality.
 */
Task_Tree_Node_List Task_Tree_Node::virtualNodes;

/**************************************************************************
 *
 * Function: void forgetVirtual (const Virtual_Address &vaddress) 
 *
 * Description: Un-memoize the node, freeing up storage
 *
 * NOTE: Put this function here so that one can link TCM programs that do not
 *       use the distributed capabilities, without having to link with IPC.
 **************************************************************************/
void Task_Tree_Node::forgetVirtual (const Virtual_Address &vaddress)
{
  Task_Tree_Node_Iterator nodeIter(virtualNodes);
  Virtual_Address virtualAddress;

  if (vaddress.host == NULL) return; // Not a real virtual address 

  for (; nodeIter; nodeIter++) {
    virtualAddress = (*nodeIter())->getVirtualAddress();
    if (vaddress.id == virtualAddress.id &&
	streq(vaddress.host, virtualAddress.host) &&
	streq(vaddress.creator, virtualAddress.creator)) {
      nodeIter.removeCurrent();
      break;
    }
  }
}
#endif /* DISTRIBUTED */
