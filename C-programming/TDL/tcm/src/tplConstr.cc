/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tplConstr.cc
 *
 * ABSTRACT: Classes for dealing with temporal constraints (essentially
 *           mapping between time points and signals/states).
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tplConstr.cc,v $ 
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
 * $Log: tplConstr.cc,v $
 * Revision 1.12  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.11  2002/06/26 16:48:23  reids
 * Made a distinction between the type-name and instance-name of a node.
 *
 * Revision 1.10  2002/03/26 05:19:53  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.9  2002/02/05 17:45:17  reids
 * Backed out the getLocal function for distributed nodes -- instead,
 *   "getVirtual" creates a new virtual node only if the "host" given as the
 *   virtual address is not the current agent (o/w the node is "local").
 * Fixed several bugs relating to race conditions in the distributed version.
 *
 * Revision 1.8  2001/07/24 12:49:04  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.7  2001/06/15 19:00:49  reids
 * Fixed a bug that David Apfelbaum found having to do with destroying a node
 *   that had a "constrainWhenAfter" event attached to it.
 *
 * Revision 1.6  2000/01/19 21:20:25  reids
 * Fixed minor compiler warning in checkInconsistent.
 *
 * Revision 1.5  1999/08/04 14:00:19  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.4  1999/06/06 13:48:10  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
// Revision 1.3  98/04/21  12:46:17  reids
// Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
//   particular event occurs.
// Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
//   a node by waiting some msecs after a particular event has occurred.
// 
// Revision 1.2  97/12/29  17:06:38  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.1  97/11/21  14:06:50  reids
// First release of TCM -- seems to be a stable version
// 
 *
 *****************************************************************************/

#include "tcmBasics.h"
#include "tplConstr.h"
#include "taskTree.h"
#include "agenda.h"
#include "tcmGlobal.h"
#include "tcmLogInterface.h"

Signal_Enum Time_Point::signalOf (void) const
{
  return stateToSignal(stateOf());
}

State_Enum Time_Point::stateOf (void) const
{
  switch (interval) {
  case Handling_Interval: return (point == Start_Point ? 
				  Handling_State : Handled_State);
  case Planning_Interval: return (point == Start_Point ? 
				  Planning_State : Planned_State);
  case Achieving_Interval: return (point == Start_Point ? 
				   Achieving_State : Achieved_State);
  default: 
    tcmError("stateOf: Cannot handle interval %d\n", interval);
    return Unknown_State;
  }
}

void constrainBefore (const Time_Point &t1, const Time_Point &t2)
{
  addEventPair(*t1.nodeOf(), t1.stateOf(), t2.signalOf(), *t2.nodeOf());

  GET_TCM_GLOBAL(loggingFns).logConstraint(t2.nodeOf(), t2.signalOf(),
					   t1.nodeOf(), t1.stateOf(), 0);
}

void constrainWhen (const Time_Point &tp, MSecs absoluteTime)
{
  addTimedEvent(absoluteTime, tp.nodeOf(), tp.signalOf());

  GET_TCM_GLOBAL(loggingFns).logConstraint(tp.nodeOf(), tp.signalOf(),
					   tp.nodeOf(), Unknown_State,
					   absoluteTime);
}

void afterSignalFn (const void *data)
{
  AFTER_CONSTRAINT_PTR afterData = (AFTER_CONSTRAINT_PTR)data;
  Timed_Event newEvent(afterData->signal, afterData->node, INFINITE_TIME);

  if (afterData->afterNode->addEventP(afterData->signal, afterData->state)) {
    GET_TCM_GLOBAL(agenda).updateTimer(newEvent, 
				       absoluteTime(afterData->waitTime),
				       TRUE);
  } else {
    GET_TCM_GLOBAL(agenda).removeTimer(newEvent, afterData->afterNode);
  }
  delete afterData;
}

void constrainWhenAfter (Task_Tree_Ref signalledNode, Signal_Enum signal,
			 Time_Point afterPoint, MSecs relativeWait,
			 const void *data)
{
  const Task_Tree_Ref signallingNode = afterPoint.nodeOf();
  State_Enum constraintState = afterPoint.stateOf();
  AFTER_CONSTRAINT_PTR afterData = new After_Constraint(signalledNode, signal, 
							relativeWait,
							signallingNode,
							constraintState);

  addTimedEvent(INFINITE_TIME, signalledNode, signal, data, signallingNode);

  GET_TCM_GLOBAL(loggingFns).logConstraint(signalledNode, signal,
					   signallingNode, constraintState,
					   relativeWait);

  // When the constraintState of the signallingNode is reached, invoke the
  // function to wait the appropriate time.
  signallingNode->addRequestedEvent(constraintState, afterSignalFn, afterData);
}

void terminateAfter (const Task_Tree_Ref &terminateRef, const Time_Point &tp)
{
  addEventPair(tp.nodeOf(), tp.stateOf(),
	       Start_Terminating_Signal, terminateRef);

  GET_TCM_GLOBAL(loggingFns).logConstraint(terminateRef, 
					   Start_Terminating_Signal,
					   tp.nodeOf(), tp.stateOf(), 0);
}

void terminateWhen (const Task_Tree_Ref &terminateRef, MSecs absoluteTime)
{
  addTimedEvent(absoluteTime, terminateRef, Start_Terminating_Signal);

  GET_TCM_GLOBAL(loggingFns).logConstraint(terminateRef, 
					   Start_Terminating_Signal,
					   terminateRef, Unknown_State,
					   absoluteTime);
}

/**************************************************************************
 *
 * FUNCTION: BOOLEAN checkInconsistent(int tplConstraints)
 *
 * DESCRIPTION: Return TRUE if the set of temporal constraints 
 *              is mutually inconsistent.
 *
 * NOTE: Adapted from TCA code.
 *
 **************************************************************************/

BOOLEAN checkInconsistent(int tplConstraints)
{
  const int inconsistency1 = (DELAY_PLANNING | PLAN_FIRST);

  return ((tplConstraints & inconsistency1) == inconsistency1);
}

void Task_Tree_Node::addConstraints(int tplConstraints)
{
  Task_Tree_Ref lastRef, thisRef(this);

  if (tplConstraints & (SEQ_ACH | SEQ_PLANNING)) {
    if (*parent != NULL && parent->children.size() > 1) {
      lastRef = parent->lastChild(TRUE);
      if (!(*lastRef)) {
      	tcmWarning("addConstraints: No last child defined for parent of node %s\n",
		   instanceName());
      } else {
      	if (tplConstraints & SEQ_ACH) {
	  // End(Ach(last)) <= Start(Ach(this))
	  Time_Point t1(End_Point, Achieving_Interval, lastRef);
	  Time_Point t2(Start_Point, Achieving_Interval, thisRef);
	  constrainBefore(t1, t2);
      	}
      	if (tplConstraints & SEQ_PLANNING) {
	  // End(Plng(last)) <= Start(Plng(this))
	  Time_Point t1(End_Point, Planning_Interval, lastRef);
	  Time_Point t2(Start_Point, Planning_Interval, thisRef);
	  constrainBefore(t1, t2);
      	}
      }
    }
  }

  if (tplConstraints & DELAY_PLANNING) {
    // Start(Ach(this)) <= Start(Plng(this))
    Time_Point t1(Start_Point, Achieving_Interval, thisRef);
    Time_Point t2(Start_Point, Planning_Interval, thisRef);
    constrainBefore(t1, t2);
  }
  if (tplConstraints & PLAN_FIRST) {
    // End(Plng(this)) <= Start(Ach(this))
    Time_Point t1(End_Point, Planning_Interval, thisRef);
    Time_Point t2(Start_Point, Achieving_Interval, thisRef);
    constrainBefore(t1, t2);
  }
}

// Don't terminate unecessarily, that is:
// Don't add the timed event if the signal is start terminating,
//       and the node is in the terminating/terminated state,
//       and the signal state is not terminating/terminated.
BOOLEAN Task_Tree_Node::addEventP(Signal_Enum signal, State_Enum state)
{
  return !((signal) == Start_Terminating_Signal && 
	   validState(Terminating_State) && 
	   !((state) == Terminating_State || (state) == Terminated_State));
}
