/**************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: agenda.h
 *
 * ABSTRACT: Defines the task agenda, which is responsible for 
 *           sending signals, executing the actions of task tree nodes, and
 *           other TBD functions.
 *
 * EXPORTS:
 *
 * $Revision: 1.17 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: agenda.h,v $
 * Revision 1.17  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.16  2003/10/23 12:18:46  reids
 * Fixed several memory leaks, including one big one caused by a bug in
 *   g++ version 3.2.3 (this necessitated a change to some tcm.h signatures,
 *   hence the new TCM minor version number -- 2.8.0)
 *
 * Revision 1.15  2003/04/17 21:08:24  da0g
 * Changed clearQueues to allow for return-when-all-work-is-done option.
 *
 * Revision 1.14  2002/09/16 22:46:18  da0g
 * Fixed runaway process consumes 100% of CPU issue.
 * Added infinite-loop detection (and overrides).
 *
 * Revision 1.13  2002/03/26 05:19:51  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.12  2002/01/18 14:20:40  reids
 * Handling signals from timers better
 *
 * Revision 1.11  2001/10/23 22:52:58  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.10  2001/07/24 12:49:02  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.9  1999/08/04 14:00:16  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
 * Revision 1.8  1998/12/16 03:08:19  reids
 * Added support for "on termination" functions.
 *   Also enabled tca.h and tcm.h to co-exist (needed to change values
 *   of several constants).
 *
 * Revision 1.7  98/10/30  11:16:31  da0g
 * Added ExternalEventBypassHandler.
 * 
 * Revision 1.6  1998/04/21 12:45:53  reids
 * Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
 *   particular event occurs.
 * Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
 *   a node by waiting some msecs after a particular event has occurred.
 *
 * Revision 1.5  97/12/30  13:38:38  reids
 * When node is terminated, signal (and remove) any timed-event in the timer
 *   queue that is for that node, regardless of when it is supposed to fire.
 * 
 * Revision 1.4  97/12/30  12:24:45  reids
 * Added option to *not* wait for timeouts and external events.
 * 
 * Revision 1.3  97/12/29  17:06:14  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.2  97/12/04  17:50:04  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:23  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 **************************************************************************/

#ifndef INCagenda
#define INCagenda

#include "tcmBasics.h"
#include "queue.h"
#include "event.h"
#include "external.h"
#include "tplConstr.h"

#define TIMER_NODE_NAME "__TCM_TIMER_NODE__"

typedef BOOLEAN (*Timed_Event_Comp)(const Timed_Event &, const Timed_Event &);
extern BOOLEAN timerCompFn(const Timed_Event &, const Timed_Event &);
typedef priority_queue<Timed_Event, Timed_Event_Comp> Timer_Queue;

typedef queue<Pending_Event> Event_Queue;

typedef queue<Task_Tree_Ref> Task_Queue;

class Agenda
{
 public:
  Agenda()
    : _timerQueue(timerCompFn),
      _externalEvents(),
      _allowInfiniteTimeouts ( FALSE )
  { _infiniteTimeTimeout.tv_sec  = 0;
    _infiniteTimeTimeout.tv_usec = 50000; }
  ~Agenda();

  void queueEvent(const Pending_Event &event) { _eventQueue.push(event); };
  void queueTask(Task_Tree_Ref const &task) { _taskQueue.push(task); };
  void queueTimer(Timed_Event const &timedEvent) 
    { _timerQueue.push(timedEvent); };
  void removeTimer(Timed_Event const &timedEvent, Task_Tree_Ref fromNode=NULL);
  void updateTimer(Timed_Event const &newEvent, MSecs oldTimeout, 
		   BOOLEAN resetAfterNode);

  void addExternalEvent (int sd, SD_CALLBACK_FN_TYPE callback, 
			 const void *clientData)
	  { _externalEvents.addExternalEvent(sd, callback, clientData); }
  void removeExternalEvent (int sd) { _externalEvents.removeExternalEvent(sd);}
  void setExternalEventBypassHandler (
			  EXTERNAL_EVENT_BYPASS_HANDLER_TYPE theBypassHandler )
		     { _externalEvents.setBypassHandler ( theBypassHandler ); }

  BOOLEAN clearQueues(Time_Point * timePoint,
		      MSecs        timeout,
		      BOOLEAN      waitingAllowed,
		      BOOLEAN      returnWhenAllWorkIsDone );

  void    clearEventQueuesIfClearQueuesNotRunning();

  BOOLEAN taskQueued(Task_Tree_Ref const &task) const
    { return _taskQueue.member(task); }

  Task_Tree_Ref getTimerNode (void);


  BOOLEAN         getAllowInfiniteTimeouts() const
				 { return _allowInfiniteTimeouts; }
  void            setAllowInfiniteTimeouts( BOOLEAN theAllowValue = TRUE )
				 { _allowInfiniteTimeouts = theAllowValue; }

  struct timeval  getInfiniteTimeTimeout() const
				 { return _infiniteTimeTimeout; }

  void            setInfiniteTimeTimeout ( struct timeval theTimeout )
				 { setAllowInfiniteTimeouts();
				   _infiniteTimeTimeout   = theTimeout; }


 private:
  Event_Queue     _eventQueue;
  Task_Queue      _taskQueue;
  Timer_Queue     _timerQueue;
  External_Events _externalEvents;
  Task_Tree_Ref   _timerNode;
  BOOLEAN         _allowInfiniteTimeouts;
  struct timeval  _infiniteTimeTimeout;

  BOOLEAN _clearEventQueue (void);
  BOOLEAN _clearTimerQueue (void);
  BOOLEAN _popTaskQueue (void);
  BOOLEAN _waitForEvents (MSecs timeout);
  BOOLEAN _doneProcessing (Time_Point *timePoint, MSecs absoluteTime)
    { return ((timePoint && timePoint->isPast()) ||
	      (absoluteTime != INFINITE_TIME && 
	       absoluteTime <= timeInMsecs())); }
  struct timeval *_selectTimeout (MSecs maxTime);
};

void addTimedEvent (MSecs absoluteTime,
		    const Task_Tree_Ref nodeRef, Signal_Enum signal,
		    const void *data=NULL, const Task_Tree_Ref afterRef=NULL);

void addTimedEvent (MSecs absoluteTime,
		    SIMPLE_CALLBACK_FN_TYPE callbackFn, const void *data);

BOOLEAN isTimerNode(Task_Tree_Ref node);

#endif /* INCagenda */
