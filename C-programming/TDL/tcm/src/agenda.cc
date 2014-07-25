/**************************************************************************
 * 
 * PROJECT: Task Control Management (TCM)
 *
 * FILE: agenda.cc
 *
 * ABSTRACT: Defines the task agenda, which is responsible for 
 *           sending signals, executing the actions of task tree nodes, and
 *           other TBD functions.
 *
 * EXPORTS:
 *
 * $Revision: 1.24 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: agenda.cc,v $
 * Revision 1.24  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.23  2003/10/23 12:18:45  reids
 * Fixed several memory leaks, including one big one caused by a bug in
 *   g++ version 3.2.3 (this necessitated a change to some tcm.h signatures,
 *   hence the new TCM minor version number -- 2.8.0)
 *
 * Revision 1.22  2003/07/30 09:10:33  da0g
 * Endless loop detection now uses the unsuppressible tcmWarning()
 *   instead of tcmMessage().
 * Increased tcmWarning() buffer size.
 *
 * Revision 1.21  2003/04/17 21:08:24  da0g
 * Changed clearQueues to allow for return-when-all-work-is-done option.
 *
 * Revision 1.20  2002/09/16 22:46:17  da0g
 * Fixed runaway process consumes 100% of CPU issue.
 * Added infinite-loop detection (and overrides).
 *
 * Revision 1.19  2002/06/26 16:48:23  reids
 * Made a distinction between the type-name and instance-name of a node.
 *
 * Revision 1.18  2002/03/26 05:19:50  reids
 * Significant bug fixes to handling of distributed nodes to take into account
 *   non-persistent task tree nodes.  Also, cleaning up of the "virtualNodes"
 *   list when task tree nodes complete, and significant change to the way
 *   "when after" constraints are handled.
 *
 * Revision 1.17  2002/01/18 14:20:41  reids
 * Handling signals from timers better
 *
 * Revision 1.16  2001/10/23 22:52:57  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.15  2001/07/24 12:49:02  reids
 * Enable monitor client data to be sent with all the monitor activation
 *   functions (ActivateAt, ActivateIn, ActivateInAfter).  Fixed bugs in
 *   the way monitor data was being set.
 * Separate templates for virtual nodes from regular templates (compiled in
 *   only when DISTRIBUTED option is used -- makes code cleaner).
 *
 * Revision 1.14  1999/08/04 14:00:16  reids
 * Distributed version of TCM.  Provides the ability for one process to
 *  add tasks, constraints, exception handlers, etc. with another process.
 *  The distributed version is created by compiling with -DDISTRIBUTED flag,
 *   which can be gotten by "gmaking" with DISTRIBUTED=1.
 *
// Revision 1.13  99/02/26  18:05:41  da0g
// Modified Agenda::clearQueues not to exit until timePoint/timeout has passed.
// 
// Revision 1.12  98/12/16  03:08:18  reids
// Added support for "on termination" functions.
//   Also enabled tca.h and tcm.h to co-exist (needed to change values
//   of several constants).
// 
// Revision 1.11  98/10/30  11:16:31  da0g
// Added ExternalEventBypassHandler.
// 
// Revision 1.10  98/10/20  22:06:23  reids
// Don't add Start_Terminating signals if signalled node is already completed.
// 
// Revision 1.9  98/07/14  17:18:32  reids
// Fixed a bug in handling Activate_Signal.
// 
// Revision 1.8  98/05/30  12:00:38  reids
// Fixed "waitFor" if timepoint is achieved but there are still events on
//   the timer queue.
// Clear timeouts related to nodes that are completely achieved.
// 
// Revision 1.7  98/04/21  17:55:16  reids
// Fixed how "forceTimeouts" decides which signals to actually send.
// 
// Revision 1.6  98/04/21  12:45:50  reids
// Added InvokeWhen/InvokeAfter functionality, to invoke a function when a
//   particular event occurs.
// Added the DelayUntilAfter/TerminateAtAfter functionality, to delay/terminate
//   a node by waiting some msecs after a particular event has occurred.
// 
// Revision 1.5  97/12/30  13:38:37  reids
// When node is terminated, signal (and remove) any timed-event in the timer
//   queue that is for that node, regardless of when it is supposed to fire.
// 
// Revision 1.4  97/12/30  12:24:47  reids
// Added option to *not* wait for timeouts and external events.
// 
// Revision 1.3  97/12/29  17:06:12  reids
// Version that has the basic functionality needed to support TDL.
// 
// Revision 1.2  97/12/04  17:50:03  reids
// Another fairly stable version (except that monitors do not quite work)
// 
// Revision 1.1  97/11/21  14:06:22  reids
// First release of TCM -- seems to be a stable version
// 
 *
 **************************************************************************/

#include "tcmBasics.h"
#include "taskTree.h"
#include "agenda.h"
#include "external.h"
#include "tcmGlobal.h"
#include "tcmThread.h"

BOOLEAN timerCompFn (const Timed_Event &timeEvent1,
		     const Timed_Event &timeEvent2)
{
#ifdef TIMER_COMP_TRACE
  tcmMessage("timerCompFn: %ld\n", 
	     timeEvent1.getTimeout() - timeEvent2.getTimeout());
#endif
  return timeEvent1 > timeEvent2;
}

inline void enqueueEvent (Timed_Event event)
{
  GET_TCM_GLOBAL(agenda).queueTimer(event);
}

/**************************************************************************
 *
 * CLASS: Agenda
 *
 **************************************************************************/

Agenda::~Agenda (void)
{
  if (*_timerNode != NULL) {
    _timerNode->removeExpectedEvent(Done_Handling_Signal, *_timerNode);
    _timerNode = NULL;
  }
}

/**************************************************************************
 *
 * FUNCTION: void removeTimer(Timed_Event const &timedEvent)
 *
 * DESCRIPTION: Remove a timed-event in the timer queue that has the same
 *              signal and toNode as the given timedEvent.
 *
 * NOTE: Removes just one such event.
 *
 **************************************************************************/

void Agenda::removeTimer(Timed_Event const &timedEvent, Task_Tree_Ref fromNode)
{
  List_Iterate<Timed_Event> timerIter(_timerQueue);
  Timed_Event queuedEvent;
  BOOLEAN found = FALSE;
  
  for (; timerIter && !found; timerIter++) {
    queuedEvent = *timerIter();
    if (timedEvent.isFunctionalInvocation()
	? (queuedEvent.isFunctionalInvocation() &&
	   queuedEvent.getCallback() == timedEvent.getCallback())
	: (!queuedEvent.isFunctionalInvocation() &&
	   queuedEvent.getSignal() == timedEvent.getSignal() &&
	   queuedEvent.getNode() == timedEvent.getNode())) {
      timerIter.removeCurrent();
      found = TRUE;
    }
  }
  if (found) {
    // This hairy code is to deal with "when after" constraints
    if (*fromNode) {
      if (fromNode->getNodeId() != queuedEvent.getNode()->getNodeId()) {
	queuedEvent.getNode()->removeExpectedEvent(queuedEvent.getSignal(),
						   getTimerNode());
      } else if (*queuedEvent.getAfterNode() &&
		 fromNode->getNodeId() == queuedEvent.getNode()->getNodeId()) {
	queuedEvent.getAfterNode()->removeRequestedEvent(queuedEvent.getSignal(),
							 queuedEvent.getNode());
      }
    }
  } else {
    tcmWarning("removeTimer: %s not found for %s\n",
	       signalName(timedEvent.getSignal()),
	       timedEvent.getNode()->instanceName());
  }
}

/**************************************************************************
 *
 * FUNCTION: void updateTimer(Timed_Event const &timedEvent, MSecs newTimeout)
 *
 * DESCRIPTION: Find the timedEvent in the timer queue that matches the
 *              event and update its timeout.
 *
 **************************************************************************/

void Agenda::updateTimer(Timed_Event const &timedEvent, MSecs newTimeout,
			 BOOLEAN resetAfterNode)
{
  List_Iterate<Timed_Event> timerIter(_timerQueue);
  Timed_Event queuedEvent;
  BOOLEAN found = FALSE;
  
  for (; timerIter && !found; timerIter++) {
    queuedEvent = *timerIter();
    if (queuedEvent.getTimeout() == timedEvent.getTimeout() &&
	(timedEvent.isFunctionalInvocation()
	 ? (queuedEvent.isFunctionalInvocation() &&
	    queuedEvent.getCallback() == timedEvent.getCallback())
	 : (!queuedEvent.isFunctionalInvocation() &&
	    queuedEvent.getSignal() == timedEvent.getSignal() &&
	    queuedEvent.getNode() == timedEvent.getNode()))) {
      timerIter.removeCurrent();
      found = TRUE;
    }
  }
  if (found) {
    queuedEvent.setTimeout(newTimeout);
    if (resetAfterNode) queuedEvent.setAfterNode(NULL);
    enqueueEvent(queuedEvent);
  } else {
    tcmWarning("updateTimer: %s not found for %s\n",
	       signalName(timedEvent.getSignal()),
	       timedEvent.getNode()->instanceName());
  }
}


/**************************************************************************
 *
 * FUNCTION: void clearQueues (Time_Point *timePoint, MSecs absoluteTime,
 *			       BOOLEAN waitingAllowed,
 *			       BOOLEAN returnWhenAllWorkIsDone )
 *
 * DESCRIPTION: Clear the event and task queues, sending signals and 
 *              executing the actions associated with tasks.
 *
 * NOTE: Return from processing if either:
 *          (1) the time point has past.
 *	 OR (2) the absoluteTime has past.
 *       OR (3) All work is done, and returnWhenAllWorkIsDone is TRUE.
 **************************************************************************/

BOOLEAN Agenda::clearQueues (Time_Point * timePoint,
			     MSecs        timeout,
			     BOOLEAN      waitingAllowed,
			     BOOLEAN      returnWhenAllWorkIsDone )
{
  struct timeval notime = {0, 0};
  BOOLEAN        foundWork = TRUE;

#ifdef THREADED
  BOOLEAN        wasAlreadyPrimaryThread;
  unsigned int   lockCount;
#endif /* THREADED */


    /* Is another thread running clearQueues? */
#ifdef THREADED
    /* Guarantee no changes (clear) until after the tcmMessage() invocation. */
  TCM_LockMasterMutex("clearQueues");
  if (SetPrimaryThread(FALSE, TRUE, FALSE, & wasAlreadyPrimaryThread) == FALSE)
  {
#ifdef TRACE_THREADS
    tcmMessage ( "clearQueues:  Invocation canceled in thread ["
		 PRINTF_THREAD_ID_STRING "].  Alternate thread ["
		 PRINTF_THREAD_ID_STRING "] is already running clearQueues.\n",
		 PRINTF_THREAD_ID(Thread::getThreadId()),
		 PRINTF_THREAD_ID(GetPrimaryThreadId() ) );
#endif /* TRACE_THREADS */
    TCM_UnlockMasterMutex("clearQueues");
    return FALSE;
  }
  else
  {
#ifdef TRACE_THREADS
    if ( wasAlreadyPrimaryThread == TRUE )
    {
      tcmMessage ( "clearQueues:  Continuing to run [NESTED] in thread ["
		   PRINTF_THREAD_ID_STRING "].\n",
		   PRINTF_THREAD_ID(Thread::getThreadId()) );
    }
    else
    {
      tcmMessage ( "clearQueues:  Running in thread ["
		   PRINTF_THREAD_ID_STRING "].\n",
		   PRINTF_THREAD_ID(Thread::getThreadId()) );
    }
#endif /* TRACE_THREADS */
    TCM_UnlockMasterMutex("clearQueues");
  }
#endif /* THREADED */


  if (_doneProcessing(timePoint, timeout))
  {
#ifdef THREADED
    if ( wasAlreadyPrimaryThread == FALSE ) ClearPrimaryThread();
#endif /* THREADED */
    return TRUE;
  }

  do
  {
    if (_externalEvents.hasEvents()) {
      _externalEvents.dispatchEvents(&notime);
      foundWork = TRUE;
    }

    if ( _clearTimerQueue() == TRUE )  foundWork = TRUE;
    if ( _clearEventQueue() == TRUE )  foundWork = TRUE;

    if (_doneProcessing(timePoint,timeout))
    {
#ifdef THREADED
      if ( wasAlreadyPrimaryThread == FALSE ) ClearPrimaryThread();
#endif /* THREADED */
      return TRUE;
    }


#ifdef THREADED
    lockCount
      = TCM_FullyUnlockMasterMutexReturningNumberOfLocks("Agenda:clearQueues");
    if ( foundWork == FALSE )
      Thread::yield();
    TCM_FullyRelockMasterMutexNumberOfLocksTimes ( lockCount,
						   "Agenda:clearQueues" );
#endif /* THREADED */



	/* The variable foundWork is used for two purposes.  Up above,
	 * it is set/used to decide whether the current thread should
	 * yield to other threads.  Down below is the only place it can
	 * be set to FALSE, and it is used to test against the results of
	 * _popTaskQueue().
	 */
    foundWork = _popTaskQueue();


	/*******************************************************************/
	/* Check for returnWhenAllWorkIsDone -- BEFORE endless loop check! */
	/*******************************************************************/

    if (   ( returnWhenAllWorkIsDone       == TRUE          )

		/* If there is no WORK to be had... */
	&& ( foundWork                     == FALSE         )
	&& ( _taskQueue.empty()            == TRUE          )
	&& ( _eventQueue.empty()           == TRUE          )
	&& ( _timerQueue.empty()           == TRUE          )

#ifdef THREADED
	&& ( HasThreadWithUnfinishedWork() == FALSE         )
#endif /* THREADED */
       )
    {
      break;
    }



	/**************************************************/
	/* Check for an endless loop -- BEFORE WE WAIT!!! */
	/**************************************************/

    if (	/* If DISTRIBUTED and TCM_EnableDistributedComm()  *
		 * has already been invoked, this is always TRUE,  *
		 * appropriately disabling this endless-loop test. */
	   ( _externalEvents.hasEvents()   == FALSE         )

		/* If there is no WORK to be had... */
	&& ( foundWork                     == FALSE         )
	&& ( getAllowInfiniteTimeouts()    == FALSE         )
	&& ( timeout                       == INFINITE_TIME )
	&& ( _taskQueue.empty()            == TRUE          )
	&& ( _eventQueue.empty()           == TRUE          )
	&& ( _timerQueue.empty()           == TRUE          )

#ifdef THREADED
	&& ( HasThreadWithUnfinishedWork() == FALSE         )
#endif /* THREADED */
       )
    {
      tcmWarning ( "[Agenda:clearQueues]  Endless loop detected: "
		   "Out of pending Tasks.  No further incoming Tasks.  "
		   "Infinite time delay.  "
		   "Canceling clearQueues to avoid infinite block (deadlock). "
		   " (Use TCM_SetAllowInfiniteTimeouts() or "
		   "TCM_SetInfiniteTimeTimeout() to disable this "
		   "cancellation policy.)\n"
		  );
      break;
    }

    if ( (foundWork == FALSE)  &&  (waitingAllowed == TRUE) )
      _waitForEvents(timeout);

	/* If we are waiting for a timePoint, or a finite time, it is  *
	 * checked up above with _doneProcessing().  So keep looping!  */

	/* Ya know, if we are waiting for INFINITE_TIME, *
	 * it seems kinda silly to stop looping...       */

  } while ( TRUE );

/* Originally was:
 *  while (    (foundWork = _popTaskQueue() )
 *	    || (waitingAllowed && _waitForEvents(timeout))
 *	   // Not done processing, but still something to wait for
 *	    || ((timePoint != NULL) || (timeout != INFINITE_TIME)));
 */


#ifdef THREADED
  if ( wasAlreadyPrimaryThread == FALSE ) ClearPrimaryThread();
#endif /* THREADED */
  return TRUE;
}


void
Agenda::clearEventQueuesIfClearQueuesNotRunning()
{
#ifdef THREADED
  TCM_LockMasterMutex("clearEventQueuesIfClearQueuesNotRunning");

	/* If Agenda::clearQueues() is not running. */
  if ( SetPrimaryThread(FALSE, FALSE, FALSE) == TRUE )
  {
    tcmMessage ( "clearEventQueuesIfClearQueuesNotRunning:  "
		 "Running in thread [" PRINTF_THREAD_ID_STRING "].\n",
		 PRINTF_THREAD_ID(Thread::getThreadId()) );

    _clearEventQueue();

    ClearPrimaryThread();
  }

  TCM_UnlockMasterMutex("clearEventQueuesIfClearQueuesNotRunning");
#endif /* THREADED */
}


/**************************************************************************
 *
 * FUNCTION: void clearEventQueue (void)
 *
 * DESCRIPTION: Send signals to nodes for all pending events on the even queue.
 *
 **************************************************************************/

BOOLEAN Agenda::_clearEventQueue (void)
{
  BOOLEAN returnValue = FALSE;

  while (!_eventQueue.empty()) {
    Pending_Event pendingEvent = *_eventQueue.front();
    _eventQueue.pop();
    pendingEvent.invokeSignal();
    returnValue = TRUE;
  }

  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: void clearTimerQueue (void)
 *
 * DESCRIPTION: Send signals to nodes for all timed events whose timeout
 *                is now (or has past).
 *
 **************************************************************************/

BOOLEAN Agenda::_clearTimerQueue (void)
{
  BOOLEAN returnValue = FALSE;

  if (!_timerQueue.empty()) {
    MSecs now = timeInMsecs();

    while (!_timerQueue.empty() && (_timerQueue.top()->getTimeout() <= now)) {
#ifdef TRACE_TIMER
      if (_timerQueue.top()->isFunctionalInvocation()) {
	tcmMessage("Signalling Functional Timed Event <%#X>\n",
		   _timerQueue.top()->getCallback());
      } else {
	tcmMessage("Signalling Timed Event <%s, %s>\n",
		   signalName(_timerQueue.top()->getSignal()),
		   _timerQueue.top()->getNode()->instanceName());
      }
#endif
      _timerQueue.top()->sendSignal(getTimerNode());
      _timerQueue.pop();
      returnValue = TRUE;
    }
  }

  return returnValue;
}

/**************************************************************************
 *
 * FUNCTION: BOOLEAN popTaskQueue (void)
 *
 * DESCRIPTION: Execute the action associated with the first task tree node
 *              on the agenda's task queue (or delete the node if it is in
 *              the terminated state, and has no other references).
 *              Return TRUE if there was such a node.
 *
 **************************************************************************/

BOOLEAN Agenda::_popTaskQueue (void)
{
  if (_taskQueue.empty()) {
    return FALSE;
  } else {
    Task_Tree_Ref const task = *_taskQueue.front();
    _taskQueue.pop();
    // Execute the node, unless it is in the process of being terminated
    //  (or if it is in the "delay termination" state, used by the 
    //  "on termination" nodes
    if (task->isCurrentState(Alive_State) || task->isTerminationDelayed()) {
      ExecuteTask ( task );
    } else if (!task->validState(Handled_State)) {
      task->doneHandling();
    }
    return TRUE;
  }
}

BOOLEAN Agenda::_waitForEvents (MSecs timeout)
{
  // Conditions under which one should wait for a timeout (or other event)
  BOOLEAN waiting = (_taskQueue.empty() && _eventQueue.empty());

    /* Originally, in addition to the above, we only waited if:
     *       _externalEvents.hasEvents()  (We have external events)
     *  || ! _timerQueue.empty()          (We have timer    events)
     *  || timeout != INFINITE_TIME       (We have a finite hold (sleep) time)
     * However, since we now detect infinite loops elsewhere (previously),
     * regardless of whether timeout == or != INFINITE_TIME, we want to wait...
     */

  if (waiting) {
    _externalEvents.dispatchEvents(_selectTimeout(timeout));
  }
  return waiting;
}

struct timeval *Agenda::_selectTimeout (MSecs maxTime)
{
  static struct timeval timeout;

  MSecs time = (   _timerQueue.empty()
		 ? INFINITE_TIME
		 : _timerQueue.top()->getTimeout() );

  if ( time > maxTime )
    time = maxTime;

  if ( time == INFINITE_TIME )
  {
	/* Do not block indefinitely -- in case user uses an *
	 * interrupt-based signal to give us new tasks.      *
	 *   (Granted, doing so would introduce all sorts    *
	 *    of really nasty race conditions, but...)       */
    timeout = getInfiniteTimeTimeout();
  }
  else
  {
    MSecs now = timeInMsecs();
    time = (time > now) ? (time - now) : 0;

    timeout.tv_sec  =  time / 1000;
    timeout.tv_usec = (time % 1000) * 1000;
  }

#ifdef THREADED
  if ( HasThreadWithUnfinishedWork() == TRUE )
  {
    if (   ( timeout.tv_sec  != 0                  )
	|| ( timeout.tv_usec >  Thread::YIELD_TIME ) )
    {
      timeout.tv_sec  = 0;
      timeout.tv_usec = Thread::YIELD_TIME;
    }
  }
#endif /* THREADED */

  return & timeout;

}

/****************************************************************
 *
 * Public Functions
 *
 *****************************************************************/

void addTimedEvent (MSecs absoluteTime,
		    const Task_Tree_Ref nodeRef, Signal_Enum signal,
		    const void *data, Task_Tree_Ref afterRef)
{
  Timed_Event timedEvent(signal, nodeRef, absoluteTime, afterRef);

  if (data != NULL) timedEvent.setData(data);

  enqueueEvent(timedEvent);
  nodeRef->addExpectedEvent(signal, GET_TCM_GLOBAL(agenda).getTimerNode());
}

void addTimedEvent (MSecs absoluteTime,
		    SIMPLE_CALLBACK_FN_TYPE callbackFn, const void *data)
{
  Timed_Event timedEvent(callbackFn, data, absoluteTime);

  enqueueEvent(timedEvent);
}

BOOLEAN isTimerNode(Task_Tree_Ref node)
{
  return streq(node->nodeTypeName(), TIMER_NODE_NAME);
}

Task_Tree_Ref Agenda::getTimerNode (void)
{
  if (_timerNode == NULL) _timerNode = new Task_Tree_Node(TIMER_NODE_NAME);
  return _timerNode;
}
