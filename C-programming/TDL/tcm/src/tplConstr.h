/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tplConstr.h
 *
 * ABSTRACT: Classes for dealing with temporal constraints (essentially
 *           mapping between time points and signals/states).
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tplConstr.h,v $ 
 * $Revision: 1.11 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tplConstr.h,v $
 * Revision 1.11  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.10  2003/10/23 12:16:10  reids
 * Deleted signature of unimplemented function
 *
 * Revision 1.9  2002/03/26 05:19:53  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.8  2001/09/07 03:45:02  reids
 * Refixed the "ConstrainWhenAfter" bug -- actually deleting the data if the
 *   signal is terminated before being sent.
 * Also NULL'ed out a task's action reference when the node is terminated.
 *   Previously, the action was deleted *after* the node was deleted.  But, if
 *   the action had an internal reference to the task itself, this would
 *   create a circular dependency in the reference counts, and the node (and
 *   hence the action) would never actually be freed.  Setting the action to
 *   NULL explicitly breaks this chain.
 *
 * Revision 1.7  2001/07/24 12:49:05  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.6  2001/06/15 19:00:49  reids
 * Fixed a bug that David Apfelbaum found having to do with destroying a node
 *   that had a "constrainWhenAfter" event attached to it.
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
 * Revision 1.3  98/04/21  12:46:18  reids
 * Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
 *   particular event occurs.
 * Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
 *   a node by waiting some msecs after a particular event has occurred.
 * 
 * Revision 1.2  97/12/04  17:50:32  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:51  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 *****************************************************************************/

#ifndef INCtplConstr
#define INCtplConstr

#include "tcm.h"
#include "event.h"
#include "taskTree.h"

class Time_Point
{
 public:
  Time_Point(TCM_Point_Enum whichPoint, TCM_Interval_Enum whichInterval,
	     Task_Tree_Ref const &whichNode)
    { point = whichPoint; interval = whichInterval; node = whichNode; }
  Time_Point(TCM_Point timePoint)
    { point = timePoint.point(); interval = timePoint.interval();
      node = timePoint.node(); }
  Task_Tree_Ref nodeOf(void) const { return node; }
  Signal_Enum signalOf(void) const;
  State_Enum stateOf(void) const;
  BOOLEAN isPast(void) const
    { return nodeOf()->validState(stateOf()); }

 private:
  TCM_Interval_Enum interval;
  TCM_Point_Enum point;
  Task_Tree_Ref node;
};

class After_Constraint
{ public:
  After_Constraint(Task_Tree_Ref theNode, Signal_Enum theSignal,
		   MSecs wait, Task_Tree_Ref theAfterNode, State_Enum theState)
    : node(theNode), afterNode(theAfterNode)
    { waitTime = wait; signal = theSignal; state = theState; }
  ~After_Constraint() { node = NULL; afterNode = NULL; }

  Task_Tree_Ref node;
  Signal_Enum   signal;
  MSecs         waitTime;
  Task_Tree_Ref afterNode;
  State_Enum    state;
};
typedef After_Constraint *AFTER_CONSTRAINT_PTR;

void constrainBefore (const Time_Point &t1, const Time_Point &t2);

void constrainWhen (const Time_Point &tp, MSecs absoluteTime);

void afterSignalFn (const void *data);

void constrainWhenAfter (Task_Tree_Ref signalledNode, Signal_Enum signal,
			 Time_Point afterPoint, MSecs relativeWait,
			 const void *data=NULL);

void terminateAfter (const Task_Tree_Ref &terminateRef, const Time_Point &tp);

void terminateWhen (const Task_Tree_Ref &terminateRef, MSecs absoluteTime);

BOOLEAN checkInconsistent(int tplConstraints);

#endif /* INCtplConstr */
