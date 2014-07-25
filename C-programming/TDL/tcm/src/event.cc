/**************************************************************************
 * 
 * PROJECT: Task Control Management
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: event.cc
 *
 * ABSTRACT: Defines signals, events (occurrence of signals), state, 
 *           and state-events (events that trigger state transitions).
 *           Main mode of communication between task tree nodes.
 *
 * EXPORTS:
 *
 * $Revision: 1.11 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: event.cc,v $
 * Revision 1.11  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.10  2009/01/06 18:10:11  reids
 * Fixed compiler warning -- had been doing a really nasty cast.
 *
 * Revision 1.9  2002/06/26 16:48:23  reids
 * Made a distinction between the type-name and instance-name of a node.
 *
 * Revision 1.8  2002/01/18 14:20:18  reids
 * Upgraded tracing options
 *
 * Revision 1.7  2001/07/24 12:49:03  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.6  1999/06/06 13:48:08  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
// Revision 1.5  98/06/02  10:39:32  reids
// Increased overall efficiency of TCM.
// 
// Revision 1.4  98/04/21  12:45:54  reids
// Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
//   particular event occurs.
// Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
//   a node by waiting some msecs after a particular event has occurred.
// 
// Revision 1.3  97/12/29  17:06:15  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.2  97/12/04  17:50:05  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:24  reids
// First release of TCM -- seems to be a stable version
// 
 *
 **************************************************************************/

#include "agenda.h"
#include "tcmGlobal.h"
#include "event.h"

/**************************************************************************
 *
 * FUNCTION: STRING signalName (Signal_Enum signal);
 *
 * DESCRIPTION: Return the string name of the given signal.
 *
 **************************************************************************/

STRING signalName (Signal_Enum signal)
{
  switch (signal) {
  case Start_Handling_Signal: return "Start_Handling";
  case Done_Handling_Signal:  return "Done_Handling";

  case Start_Planning_Signal: return "Start_Planning";
  case Done_Planning_Signal:  return "Done_Planning";

  case Start_Achieving_Signal: return "Start_Achieving";
  case Done_Achieving_Signal:  return "Done_Achieving";

  case Start_Terminating_Signal: return "Start_Terminating";
  case Done_Terminating_Signal:  return "Done_Terminating";

  case Activate_Signal: return "Activate";
  case Timeout_Signal: return "Timeout";

  default: return "Unknown";
  }
}

/**************************************************************************
 *
 * FUNCTION: STRING stateName (State_Enum state);
 *
 * DESCRIPTION: Return the string name of the given state.
 *
 **************************************************************************/

STRING stateName (State_Enum state)
{
  switch (state) {
  case Unhandled_State: return "Unhandled";
  case Handling_State:  return "Handling";
  case Handled_State:   return "Handled";

  case Unplanned_State: return "Unplanned";
  case Planning_State:  return "Planning";
  case Planned_State:   return "Planned";

  case Unachieved_State: return "Unachieved";
  case Achieving_State:  return "Achieving";
  case Achieved_State:   return "Achieved";

  case Alive_State:   return "Alive";
  case Terminating_State: return "Terminating";
  case Terminated_State:  return "Terminated";

  default: return "Unknown";
  }
}

/**************************************************************************
 *
 * FUNCTION: Signal_Enum stateToSignal(State_Enum state);
 *
 * DESCRIPTION: Return the signal corresponding to the given state.
 *
 **************************************************************************/

Signal_Enum stateToSignal(State_Enum state)
{
  switch (state) {
  case Handling_State:  return Start_Handling_Signal;
  case Handled_State:   return Done_Handling_Signal;

  case Planning_State:  return Start_Planning_Signal;
  case Planned_State:   return Done_Planning_Signal;

  case Achieving_State:  return Start_Achieving_Signal;
  case Achieved_State:   return Done_Achieving_Signal;

  case Terminating_State: return Start_Terminating_Signal;
  case Terminated_State:  return Done_Terminating_Signal;

  case Unknown_State: return Unknown_Signal;

  default:
    tcmError("stateToSignal: No corresponding signal for %s\n", 
	     stateName(state));
    return Unknown_Signal;
  }
}

/**************************************************************************
 *
 * FUNCTION: State_Enum signalToState(Signal_Enum signal)
 *
 * DESCRIPTION: Return the state corresponding to the given signal.
 *
 **************************************************************************/

State_Enum signalToState(Signal_Enum signal)
{
  switch (signal) {
  case Start_Handling_Signal: return Handling_State;
  case Done_Handling_Signal:  return Handled_State;

  case Start_Planning_Signal: return Planning_State;
  case Done_Planning_Signal:  return Planned_State;

  case Start_Achieving_Signal: return Achieving_State;
  case Done_Achieving_Signal:  return Achieved_State;

  case Start_Terminating_Signal: return Terminating_State;
  case Done_Terminating_Signal:  return Terminated_State;

  default: return Unknown_State;
  }
}

/**************************************************************************
 *
 * FUNCTION: Phase_Enum stateToPhase(State_Enum state)
 *
 * DESCRIPTION: Return the phase corresponding to the given state.
 *
 **************************************************************************/
Phase_Enum stateToPhase(State_Enum state)
{
  static Phase_Enum stateToPhaseArray[] = 
    { Terminating_Phase, Handling_Phase, Handling_Phase, Handling_Phase,
      Planning_Phase, Planning_Phase, Planning_Phase,
      Achieving_Phase, Achieving_Phase, Achieving_Phase,
      Terminating_Phase, Terminating_Phase, Terminating_Phase};

  if (state <= Unknown_State || state > Terminated_State) {
    tcmError("stateToPhase: Unhandled state: %d\n", state); 
    return Terminating_Phase; // To keep compiler happy
  } else {
    return stateToPhaseArray[state];
  }
}

/**************************************************************************
 *
 * CLASS: State
 *
 * Description: A tuple of <handling, achieving, planning, terminating>
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void set(State_Enum state)
 *
 * DESCRIPTION: Set the appropriate value of the state tuple.
 *
 **************************************************************************/

void State::set(State_Enum state)
{
  phases[stateToPhase(state)] = state;
}

/**************************************************************************
 *
 * FUNCTION: State_Enum next (State_Enum state)
 *
 * DESCRIPTION: Get the next value of the appropriate state tuple.
 *
 **************************************************************************/

State_Enum  State::next(State_Enum state) const
{
  switch (state) {
  case Unhandled_State: return Handling_State;
  case Handling_State:  return Handled_State;

  case Unplanned_State: return Planning_State;
  case Planning_State:  return Planned_State;

  case Unachieved_State: return Achieving_State;
  case Achieving_State:  return Achieved_State;

  case Alive_State:   return Terminating_State;
  case Terminating_State: return Terminated_State;

  default: 
    // tcmError("State::next: No next state after %s\n", stateName(state));
    return Unknown_State;
  }
}

/**************************************************************************
 *
 * FUNCTION: void compare(State_Enum state)
 *
 * DESCRIPTION: Compare the current state to the appropriate value of
 *              the state tuple.  Return -1 if state is less than the current
 *              state, 0 if they are equal, 1 if state is greater than current.
 *
 **************************************************************************/

int State::compare(State_Enum state) const
{
  State_Enum currentState = current(state);

  return (state < currentState ? -1 : state > currentState ? 1 : 0);
}

#if 0 /* Inline this function */
/**************************************************************************
 *
 * FUNCTION: State_Enum current(State_Enum state)
 *
 * DESCRIPTION: Return the phase of the state tuple corresponding to the
 *              given state (e.g., if "state" is Unplanned_State, return
 *              the Planning_Phase).
 *
 **************************************************************************/

State_Enum State::current(State_Enum state) const
{
  return phases[stateToPhase(state)];
}
#endif

/**************************************************************************
 *
 * CLASS: Event
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void sendSignal(Task_Tree_Ref const &fromNode) const
 *
 * DESCRIPTION: Given a <signal, node> event, send the signal to the node
 *              (from the given fromNode).
 *
 * NOTE: Actually puts signals on the agenda, rather than sending them right
 *       away, to avoid possibility of unbounded computation occurring.
 *
 **************************************************************************/

void Event::sendSignal(Task_Tree_Ref const &fromNode) const
{
  if (isFunctionalInvocation()) {
    Pending_Event event((DATA_CALLBACK_FN_TYPE)callback, data, fromNode);

#ifdef TRACE_SIGNALS
    tcmMessage("%s signalling function <%#X>\n",
	       fromNode->instanceName(), getCallback());
#endif
    GET_TCM_GLOBAL(agenda).queueEvent(event);
  } else {
    Pending_Event event(signal, node, fromNode);

    if (data != NULL) event.setData(data);

#ifdef TRACE_SIGNALS
    tcmMessage("%s sending signal %s to %s\n",
	       fromNode->instanceName(), signalName(getSignal()),
	       getNode()->instanceName());
#endif
    GET_TCM_GLOBAL(agenda).queueEvent(event);
  }
}

/**************************************************************************
 *
 * CLASS: Pending_Event
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: void invokeSignal (void)
 *
 * DESCRIPTION: Send the signal from the "fromNode" to the "toNode"
 *
 **************************************************************************/

void Pending_Event::invokeSignal (void) const
{
  if (isFunctionalInvocation()) {
    (*(DATA_CALLBACK_FN_TYPE)callback)(data, getFromNode());
  } else {
    Event signalledEvent(getSignal(), getFromNode());

    if (data != NULL) signalledEvent.setData(data);

    getToNode()->signal(signalledEvent);
  }
}

/**************************************************************************
 *
 * CLASS: Event_List
 *
 * DESCRIPTION: A list of event's
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: int count(Signal_Enum signal)
 *
 * DESCRIPTION: Return the number of events in the list that have the 
 *              given signal.
 *
 **************************************************************************/

unsigned int Event_List::count(Signal_Enum signal) const
{
  unsigned int numSignals = 0;

  Const_Event_Iterator listIter(this);
  for (listIter.reset(); listIter; listIter++) {
    if (signal == listIter()->getSignal()) numSignals++;
  }

  return numSignals;
}

/**************************************************************************
 *
 * CLASS: State_Event_List
 *
 * DESCRIPTION: A list of state_event's
 *
 **************************************************************************/

/**************************************************************************
 *
 * FUNCTION: int count(Signal_Enum signal)
 *
 * DESCRIPTION: Return the number of state_events in the list that have the 
 *              given signal.
 *
 **************************************************************************/

unsigned int State_Event_List::count(Signal_Enum signal) const
{
  unsigned int numSignals = 0;

  Const_State_Event_Iterator listIter(this);
  for (listIter.reset(); listIter; listIter++) {
    if (signal == listIter()->getSignal()) numSignals++;
  }

  return numSignals;
}

#include "stdio.h"

// Debugging help
void printEventList(Event_List const &eventList); // Prototype
void printEventList(Event_List const &eventList)
{
  Const_Event_Iterator listIter(&eventList);
  for (listIter.reset(); listIter; listIter++) {
    tcmMessage("<%s, %s>\n", signalName(listIter()->getSignal()),
	       listIter()->getNode()->instanceName());
  }
}

void printStateEventList(State_Event_List const &eventList); // Prototype
void printStateEventList(State_Event_List const &eventList)
{
  Const_State_Event_Iterator listIter(eventList);
  for (listIter.reset(); listIter; listIter++) {
    tcmMessage("<%s, %s, %s>\n", signalName(listIter()->getSignal()),
	       listIter()->getNode()->instanceName(), 
	       stateName(listIter()->getState()));
  }
}
