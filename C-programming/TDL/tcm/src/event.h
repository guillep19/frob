/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: event.h
 *
 * ABSTRACT: Defines signals, events (occurrence of signals), state, 
 *           and state-events (events that trigger state transitions).
 *           Main mode of communication between task tree nodes.
 *
 * EXPORTS:
 *
 * $Revision: 1.12 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: event.h,v $
 * Revision 1.12  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.11  2009/01/06 18:10:11  reids
 * Fixed compiler warning -- had been doing a really nasty cast.
 *
 * Revision 1.10  2002/03/26 05:19:52  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.9  2002/01/18 14:21:10  reids
 * Removed "sendSignal(void)" -- no longer needed
 *
 * Revision 1.8  2001/07/24 12:49:03  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.7  2001/03/26 21:38:56  trey
 * changed list<T> type to be tcmList<T> to avoid conflict with STL lists
 *
 * Revision 1.6  1999/10/21 21:32:31  reids
 * Changed list.cc and list.h to tcmList.cc and tcmList.h to avoid conflicts
 *   with other occurrences of those names in other packages.
 *
 * Revision 1.5  1999/08/04 14:00:17  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.4  98/06/02  10:39:34  reids
 * Increased overall efficiency of TCM.
 * 
 * Revision 1.3  98/04/21  12:45:56  reids
 * Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
 *   particular event occurs.
 * Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
 *   a node by waiting some msecs after a particular event has occurred.
 * 
 * Revision 1.2  97/12/04  17:50:06  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:27  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INCevent
#define INCevent

#include "tcmBasics.h"
#include "tcmList.h"
#include "tcm.h"
#include "tcmPriv.h"

enum Signal_Enum { Unknown_Signal,
		   Start_Handling_Signal, Done_Handling_Signal,
		   Start_Planning_Signal, Done_Planning_Signal,
		   Start_Achieving_Signal, Done_Achieving_Signal,
		   Start_Terminating_Signal, Done_Terminating_Signal,
		   Activate_Signal, Timeout_Signal};

enum State_Enum { Unknown_State, 
		  Unhandled_State, Handling_State, Handled_State,
		  Unplanned_State, Planning_State, Planned_State,
		  Unachieved_State, Achieving_State, Achieved_State,
		  Alive_State, Terminating_State, Terminated_State};

enum Phase_Enum { Handling_Phase, Planning_Phase, Achieving_Phase, 
		  Terminating_Phase};

STRING signalName (Signal_Enum signal);

STRING stateName (State_Enum state);

State_Enum signalToState(Signal_Enum signal);
Signal_Enum stateToSignal(State_Enum state);

Phase_Enum stateToPhase(State_Enum state);

class State
{
 public:
  State() { phases[Handling_Phase] = Unhandled_State;
	    phases[Achieving_Phase] = Unachieved_State;
	    phases[Planning_Phase] = Unplanned_State; 
	    phases[Terminating_Phase] = Alive_State; }
  void set (State_Enum state);
  State_Enum next (State_Enum state) const ;
  int compare (State_Enum state) const;
  State_Enum current (State_Enum state) const 
    { return phases[stateToPhase(state)]; }

 private:
  State_Enum phases[4];
};

// This is pretty kludgy, but could not think of a better way that didn't
// involve creating new objects, which I want to avoid since Event's are
// very common.
// There are two flavors of events -- "node" events signal task tree nodes;
//  "functional" events merely invoke functions.

class Invocation
{
 public:
  Invocation()
    : signal(Unknown_Signal), node(NULL), callback(NULL) { data = NULL; }
  Invocation(Signal_Enum theSignal, Task_Tree_Ref const &theNode)
    : signal(theSignal), node(theNode), callback(NULL) { data = NULL; }
  Invocation(const SIMPLE_CALLBACK_FN_TYPE callbackFn, 
	     const void *callbackData = NULL) 
    : signal(Unknown_Signal), node(NULL), callback(callbackFn) 
    { data = callbackData; }

  // Differentiate "node" events from "functional" events
  BOOLEAN isFunctionalInvocation(void) const { return callback != NULL; }

  Signal_Enum getSignal() const { return signal; }
  Task_Tree_Ref const &getNode() const { return node; }

  SIMPLE_CALLBACK_FN_TYPE getCallback() const { return callback; }
  const void *getData() const { return data; }

  BOOLEAN operator==(const Invocation &invocation1) const
    { return (isFunctionalInvocation()
	      ? (callback == invocation1.getCallback() &&
		 data == invocation1.getData())
	      : (node == invocation1.getNode() &&
		 signal == invocation1.getSignal())); }

 protected:
  // For "node" events
  Signal_Enum signal;
  Task_Tree_Ref node;
  // For "functional" events
  SIMPLE_CALLBACK_FN_TYPE callback;
  const void *data;
};

typedef void (*DATA_CALLBACK_FN_TYPE)(const void *callbackData,
				      const TCM_Task_Tree_Ref &nodeRef);

class Event : public Invocation
{
 public:
  Event(void) {}
  Event(Signal_Enum theSignal, Task_Tree_Ref const &theNode)
    : Invocation(theSignal, theNode) {}
  Event(SIMPLE_CALLBACK_FN_TYPE callbackFn, const void *callbackData)
    : Invocation(callbackFn, callbackData) { }
  Event(DATA_CALLBACK_FN_TYPE callbackFn, const void *callbackData)
    : Invocation((SIMPLE_CALLBACK_FN_TYPE)callbackFn, callbackData) { }

  void setData (const void *theData) { data = theData; }

  void sendSignal(Task_Tree_Ref const &fromNode) const;
};

class State_Event : public Event
{
 public:
  State_Event(void) { state = Unknown_State; }
  State_Event(Signal_Enum theSignal, Task_Tree_Ref const &theNode,
	      State_Enum theState)
    : Event(theSignal, theNode) { state = theState; }
  State_Event(Signal_Enum theSignal, Task_Tree_Ref const &theNode,
	      State_Enum theState, void const *theData)
    : Event(theSignal, theNode) { state = theState; data = theData; }
  State_Event(SIMPLE_CALLBACK_FN_TYPE callbackFn, const void *callbackData,
	      State_Enum theState)
    : Event(callbackFn, callbackData) { state = theState; }

  BOOLEAN operator==(const State_Event &event1) const
    { return ((state == event1.getState()) && 
	      this->Event::operator==(event1)); }

  State_Enum getState(void) const { return state; }

 private:
  State_Enum state;
};

class Pending_Event : public Event
{
 public:
  Pending_Event() : fromNode(NULL) {}
  Pending_Event(Signal_Enum theSignal, Task_Tree_Ref const &toNode,
		Task_Tree_Ref const &theFromNode)
    : Event(theSignal, toNode), fromNode(theFromNode) {}
  Pending_Event(DATA_CALLBACK_FN_TYPE callbackFn, const void *callbackData,
		Task_Tree_Ref const &theFromNode)
    : Event(callbackFn, callbackData), fromNode(theFromNode) {}

  BOOLEAN operator==(const Pending_Event &event1) const
    { return ((fromNode == event1.getFromNode()) &&
	      this->Event::operator==(event1)); }

  Task_Tree_Ref const &getFromNode() const { return fromNode; };
  Task_Tree_Ref const &getToNode() const { return getNode(); };

  void invokeSignal (void) const;

 private:
  Task_Tree_Ref fromNode;
};

/****************************************************************
 *
 * "Timed_Event"s are events that trigger at a given (absolute) time.
 * They are managed by the agenda's timer-queue (a priority queue).
 *
 *****************************************************************/

class Timed_Event : public Event
{
 public:
  Timed_Event() { _timeout = 0; _afterNode = NULL; }
  Timed_Event(Signal_Enum signal, Task_Tree_Ref const &node, MSecs timeout)
    : Event(signal, node) { _timeout = timeout; }
  Timed_Event(SIMPLE_CALLBACK_FN_TYPE callbackFn, const void *callbackData,
	      MSecs timeout)
    : Event(callbackFn, callbackData) { _timeout = timeout; }
  Timed_Event(Signal_Enum signal, Task_Tree_Ref const &node, MSecs timeout,
	      Task_Tree_Ref afterNode)
    : Event(signal, node), _afterNode(afterNode) { _timeout = timeout; }

  BOOLEAN operator< (Timed_Event const &timedEvent1) const
    { return _timeout < timedEvent1._timeout; }
  BOOLEAN operator> (Timed_Event const &timedEvent1) const
    { return _timeout > timedEvent1._timeout; }

  void setTimeout (MSecs newTimeout) { _timeout = newTimeout; }
  MSecs getTimeout (void) const { return _timeout; }

  Task_Tree_Ref getAfterNode (void) const { return _afterNode; }
  void setAfterNode (Task_Tree_Ref afterNode) { _afterNode = afterNode; }

 private:
  MSecs _timeout;
  Task_Tree_Ref _afterNode;
};

class Event_List : public tcmList<Event>
{
 public:
  unsigned int count(Signal_Enum signal) const ;

  void insert(Event const &event) { insertFirst(event); }
};

class State_Event_List : public tcmList<State_Event>
{
 public:
  unsigned int count(Signal_Enum signal) const ;

  void insert(State_Event const &stateEvent) { insertFirst(stateEvent); }
};

typedef Const_List_Iterate<Event> Const_Event_Iterator;
typedef List_Iterate<Event> Event_Iterator;

typedef Const_List_Iterate<State_Event> Const_State_Event_Iterator;
typedef List_Iterate<State_Event> State_Event_Iterator;

#ifdef macintosh
class Timed_Event_List : public tcmList<Timed_Event> {};
#endif

#endif // INCevent
