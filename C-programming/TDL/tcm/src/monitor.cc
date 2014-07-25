/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: monitor.cc
 *
 * ABSTRACT: Classes for implementing monitors (point, polling, demon, event)
 *
 * EXPORTS:
 *
 * $Revision: 1.18 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: monitor.cc,v $
 * Revision 1.18  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.17  2002/06/26 16:48:23  reids
 * Made a distinction between the type-name and instance-name of a node.
 *
 * Revision 1.16  2002/03/26 05:19:52  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.15  2002/01/18 14:20:18  reids
 * Upgraded tracing options
 *
 * Revision 1.14  2001/10/23 22:52:58  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.13  2001/08/07 23:51:53  da0g
 * Fixed:  Renamed removeRequestedEvents to removeRequestedEvent.
 *
 * Revision 1.12  2001/07/24 12:49:03  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.11  2001/07/23 16:22:11  reids
 * removeRequestEvents => removeRequestEvent (just delete one event, not all).
 *
 * Revision 1.10  2000/07/05 23:09:06  da0g
 * Added rudimentary Run-Time-Type-Identification to _Action, Monitor_Action,
 * and Polling_Monitor_Action.
 *
 * Modified monitor actions & tcm.h monitor interface to support the
 * allocate-then-set-attributes model of TDL.
 *
 * Polling_Monitor_Action with an infinite time period will behave as
 * standard non-polling Monitor_Action.
 *
 * Added kludge to Polling_Monitor_Action's to detect termination
 * and force the monitor to stop running.
 *
 * Fixed bug with _initialWait being reset incorrectly.
 *
 * Fixed delete of const pointer bug.
 *
 * Revision 1.9  1999/08/04 14:00:18  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
// Revision 1.8  98/04/21  12:45:58  reids
// Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
//   particular event occurs.
// Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
//   a node by waiting some msecs after a particular event has occurred.
// 
// Revision 1.7  98/01/30  14:50:52  reids
// Updated to compile under gcc 2.7.2 and under Linux.
// Also, made STRING "const char *" and changed API to take const arguments,
//   where applicable.
// 
// Revision 1.6  97/12/30  12:28:12  reids
// Added a "timer" monitor, which is like a polling monitor, except when
//   activated it just invokes a function (rather than adding a new node)
// 
// Revision 1.5  97/12/29  17:06:23  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.4  97/12/22  16:52:54  reids
// Basically, added "data" field for CALLBACK_ACTION,
//  and started using "nodeData" for the activation data associated
//  with monitors, and the failure data associated with exceptions.
// 
// Revision 1.3  97/12/18  00:21:44  reids
// Changing ACTION_PTR to a handle, to aid in garbage collection.
// 
// Revision 1.2  97/12/04  17:50:12  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:33  reids
// First release of TCM -- seems to be a stable version
// 
 *
 **************************************************************************/

#include <stdio.h>
#include "agenda.h"
#include "tcmGlobal.h"
#include "monitor.h"

/**************************************************************************
 *
 * CLASS: Monitor_Action
 *
 * Description: A specialization of _Action, containing additional information
 *              needed to specify the action of a monitor.
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void activate (Task_Tree_Ref const &node, const void *data)
 *
 * DESCRIPTION: Unless activated enough times already, add a new activation
 *              (command) node as a child of the monitor.
 *
 **************************************************************************/

void Monitor_Action::activate (Task_Tree_Ref const &node)
{
#ifdef TRACE_ACTIVATIONS
  tcmMessage("Monitor_Action::activate: %d\n", _numActivations);
#endif
  if (_numActivations < _maxActivations) {
    if ( _activationAction == (_Action *)NULL )
    {
      tcmWarning ( "activate: Trying to activate a monitor "
		   "without an Activation-Action\n" );
      return;
    }
    _numActivations++;
    Task_Tree_Command_Node *activationNode
      = new Task_Tree_Command_Node(_activationName);
    activationNode->setAction(_activationAction);
    activationNode->setNodeData(node->getNodeData());
    node->setNodeData(NULL); // Clear monitor data for the next time around

#ifdef THREADED
	/* If we are threaded, make the activationNode threaded too. */
    activationNode -> setIsThreaded ( node -> getIsThreaded() );
#endif

    taskTreeInsert(*node, activationNode, TRUE);
    // Need to insert the constraints *after* the node has been inserted...
    activationNode->addConstraints(_activationConstraints);

    if (_numActivations == _maxActivations) {
      node->doneHandling();
    }
  } else {
    tcmWarning("activate: Trying to activate an inactive monitor\n");
  }
}

/**************************************************************************
 *
 * CLASS: Polling_Monitor_Action
 *
 * Description: A specialization of Monitor_Action, containing additional 
 *              information needed to specify the action of a polling monitor.
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void execute (Task_Tree_Ref const &node)
 *
 * DESCRIPTION: Activate the monitor and set the polling timer.
 *
 **************************************************************************/

void Polling_Monitor_Action::execute (Task_Tree_Ref const &node)
{
	/* If this is a Polling_Monitor_Action being used as *
	 * a regular Monitor_Action, do nothing...           */
  if ( getPeriod() == INFINITE_TIME )
  {
    /* Monitor_Action::execute ( node ); -- this does nothing... */
    return;
  }

	/* This is an absolute kludge to catch, and force, *
	 * termination for periodic monitors...            */
  if ( node -> isCurrentState ( Alive_State ) == FALSE )
  {
    tcmWarning ( "Polling Monitor terminated during activation." );
    if (!node->validState(Handled_State)) node->doneHandling();
    return;
  }

  if ( _initialWait == FALSE ) {
    activate(node);
  }
  _initialWait = FALSE;

  if (_numActivations < _maxActivations)
    addTimedEvent(absoluteTime(_period), node, Timeout_Signal);
}

/**************************************************************************
 *
 * CLASS: Task_Tree_Monitor_Node
 *
 * Description: A specialization of Task_Tree_Command_Node, containing 
 *              additional information needed to specify a monitor.
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void trigger (void)
 *
 * DESCRIPTION: Increment the number of times this monitor has been 
 *              triggered.  Complete handling the monitor if it has been
 *              triggered enough times, and is not already done (e.g.,
 *              may have been activated maxActivation times already).
 *
 **************************************************************************/

void Task_Tree_Monitor_Node::trigger (void)
{
  _numTriggers++;
  if (_numTriggers == _maxTriggers) {
    if (!validState(Handled_State)) doneHandling();
  }
}

/**************************************************************************
 *
 * FUNCTION: void activate (const void *data)
 *
 * DESCRIPTION: Activate the monitor (invoking the activation action).
 *
 **************************************************************************/

void Task_Tree_Monitor_Node::activate (const void *data)
{
  if (isCurrentState(Handling_State)) {
    setNodeData(data);
    ((Monitor_Action *)(*_action))->activate(this);
  } else {
    tcmWarning("Trying to activate an inactive monitor %s\n",
	       instanceName());
  }
}

/**************************************************************************
 *
 * FUNCTION: void _handleSignal(Signal_Enum signal)
 *
 * DESCRIPTION: A signal just arrived at the monitor; Handle it.
 *              Differs from handling signals for regular nodes in
 *              that it handles Activate and Timeout signals.
 *
 **************************************************************************/

void Task_Tree_Monitor_Node::_handleSignal(Signal_Enum signal)
{
  switch (signal) {
  case Activate_Signal: 
#ifdef TRACE_ACTIVATIONS
    tcmMessage("Task_Tree_Monitor_Node::_handleSignal: Activation\n");
#endif
    activate(_nodeData);
    break;

  case Timeout_Signal: 
#ifdef TRACE_ACTIVATIONS
    tcmMessage("Task_Tree_Monitor_Node::_handleSignal: Timeout\n");
#endif
    if (isCurrentState(Handling_State)) {
      execute();
    } else {
      tcmWarning("Trying to execute an inactive monitor %s\n",
		 instanceName());
    }
    break;
    
  case Start_Terminating_Signal:
    // Force node to be handled, so that it terminates correctly
    if (!validState(Handled_State)) doneHandling();
    Task_Tree_Command_Node::_handleSignal(signal);
    break;
    
  case Done_Handling_Signal:
		Task_Tree_Command_Node::_handleSignal(signal);
		// Remove all activation and timeout signals from handled monitors
		_finishHandlingMonitor();		
		 break;

  default:
    Task_Tree_Command_Node::_handleSignal(signal); break;
  }
}

/**************************************************************************
 *
 * FUNCTION: void _finishHandlingMonitor(void)
 *
 * DESCRIPTION: Remove all activation and timeout signals from handled monitors
 *
 **************************************************************************/

void Task_Tree_Monitor_Node::_finishHandlingMonitor (void)
{
  Event_Iterator expectIter(expectedEvents);
  Task_Tree_Node_Ptr expectNode = NULL;
  Signal_Enum expectSignal;
	
  // I don't think the current list implementation will work right 
  // if "removeRequestedEvent" removes something from the list being
  // iterated on here.  This is wasteful, but should work all the time.
  for (expectIter.reset(); expectIter; (expectNode == this 
					? (expectNode=NULL, expectIter.reset())
					: expectIter++)) {
    expectSignal = expectIter()->getSignal();
    if (expectSignal == Activate_Signal || expectSignal == Timeout_Signal) {
      expectNode = *(expectIter()->getNode());
      removeRequestedEvent(*expectIter());
      expectIter.removeCurrent();
    }
  }
}

/**************************************************************************
 *
 * Public Functions
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: Task_Tree_Monitor_Node *monitorDereference (TCM_Task_Tree_Ref const &nodeRef)
 *
 * DESCRIPTION: Return the monitor node (either the node itself, or its parent)
 *
 **************************************************************************/

Task_Tree_Monitor_Node *monitorDereference (TCM_Task_Tree_Ref const &nodeRef)
{
  if (nodeRef->isMonitor()) {
    return (Task_Tree_Monitor_Node *)*nodeRef;
  } else if (nodeRef->getParent()->isMonitor()) {
    return (Task_Tree_Monitor_Node *)*nodeRef->getParent();
  } else {
    return NULL;
  }
}
