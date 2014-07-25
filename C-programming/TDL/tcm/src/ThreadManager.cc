
/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 2001 Reid Simmons.  All rights reserved.
 *
 * FILE: ThreadManager.cc
 *
 * ABSTRACT: The threading subsystem.
 *           Compiled in [used] only when THREADED is defined.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/ThreadManager.cc,v $ 
 * $Revision: 1.3 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: ThreadManager.cc,v $
 * Revision 1.3  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.2  2002/12/23 02:24:07  da0g
 * Overrides User-Specified (Resume) Task-Thread mappings, if necessary.
 * Assorted cleanup.  Added Mutexes to reduce race conditions.
 * setNumberOfThreads() is now forced to be non-reentrant. Etc.
 * Added mutexed getCurrentTaskRef()
 * Added getTaskTreeRefForThisThread()
 *
 * Revision 1.1  2001/10/23 22:52:57  da0g
 * Added Threading support.  Cached lastchild.
 *
 *
 *****************************************************************************/

#include "ThreadManager.h"
#include "taskTree.h"
#include "tcmThread.h"


/*virtual*/
TaskExecutionThread::~TaskExecutionThread()
{
  if ( getThreadStatus() == Thread::RUNNING )
  {
    cerr << "[TaskExecutionThread::~TaskExecutionThread]  Warning:  "
	 << "Waiting for thread to finish." << endl;
    endThreadAfterCurrentTask();
    waitForThreadToStop();
    cerr << "[TaskExecutionThread::~TaskExecutionThread]  Status:  "
	 << "Thread finished." << endl;
  }
}




 /* The loop/code of this thread. */
/*virtual*/
void TaskExecutionThread::run()
{
	/* So we can unlock it in the first loop iteration. */
  getThreadBusyMutex().lock("TaskExecutionThread:TaskExecutionThread");

  BOOLEAN threadStillBusy = FALSE;

  while ( TRUE )
  {
    if ( threadStillBusy == FALSE )
    {
	/* We are only unlocked while waiting for work. */
      getThreadBusyMutex().unlock("TaskExecutionThread:run");

	/* Let any watchers know that we are now waiting for work. */
      getFinishedCurrentWorkSemaphore()
	. wakeupOtherThread ( Semaphore::THREAD_NOT_YET_WAITING_ALLOWED );

	/* Block until work arrives */
      getWorkSemaphore()
	. waitForSignal ( Semaphore::WAKEUP_ALREADY_HAPPENED_ALLOWED );

	/* We have work, so we are locked while handling that work. */
      getThreadBusyMutex().lock("TaskExecutionThread:run");

	/* Let the other thread know we have reached the "locked/busy" state */
      getStartedCurrentWorkSemaphore()
	. wakeupOtherThread ( Semaphore::THREAD_NOT_YET_WAITING_ALLOWED );
    }

	/* Clear that wakeup so it doesn't trigger us again. */
    getWorkSemaphore() . clearWakeupAlreadyReceived();

	/* Run our Task... */
    if ( getCurrentTaskRef() . isNotNull() )
    {
	/* Override any user-specified Task-Thread mappings, if necessary */
      TCM_PushUserTaskForThisThreadForDurationOfObject
	ifNecessaryOverrideUserTaskThreadMappingWithSystemDefault;
      
      getCurrentTaskRef() -> execute();

	/*Cancel override of user-specified Task-Thread mapping, if necessary*/
      ifNecessaryOverrideUserTaskThreadMappingWithSystemDefault . cleanup();

      getCurrentTaskRefMutex().lock("TaskExecutionThread:run");
      getCurrentTaskRefNoMutex() . clear();
      getCurrentTaskRefMutex().unlock("TaskExecutionThread:run");
    }

	/* To avoid a race condition, should-end test *MUST* occur after
	 * getWorkSemaphore() . clearWakeupAlreadyReceived();
	 */
    if ( getShouldEndMutexed() == TRUE )
      break;

	/* Try to forcibly obtain new work -- if anything is pending. */
    threadStillBusy
      = ThreadManager::getGlobalThreadManager() . threadRePopTaskQueue (this);

  } /* while ( TRUE ) */

  getThreadBusyMutex().unlock("TaskExecutionThread:TaskExecutionThread");
}


	/* Interface -- This is called by the Main Thread only! */
status_t
TaskExecutionThread::executeTask (
		   const Task_Tree_Ref & theNewTaskRef,
		   BOOLEAN               theIsRePopScenario /* = FALSE */ )
{
	/* In the event that this thread, rather than the main thread,
	 * has found a pending threaded task and wishes to undertake it,
	 * we don't lock the mutex.
	 */
  if ( theIsRePopScenario == FALSE )
  {
    getShouldEndMutex().lock  ("TaskExecutionThread:executeTask");
    if ( getShouldEndNoMutex() == TRUE )
    {
      getShouldEndMutex().unlock  ("TaskExecutionThread:executeTask");
      return FAILURE;
    }

	/* If we can't execute the task, we return failure.
	 * This resolves all sorts of race conditions right here!
	 */
    if ( getThreadBusyMutex().trylock("TaskExecutionThread:executeTask")
	 == Mutex::ALREADY_LOCKED )
    {
      getShouldEndMutex().unlock  ("TaskExecutionThread:executeTask");
      return FAILURE;
    }
  }

	/* Work is pending.  We are not finished. */
  getFinishedCurrentWorkSemaphore() . clearWakeupAlreadyReceived();

  getCurrentTaskRefMutex().lock("TaskExecutionThread:executeTask");
  getCurrentTaskRefNoMutex() = theNewTaskRef;
  getCurrentTaskRefMutex().unlock("TaskExecutionThread:executeTask");


  if ( theIsRePopScenario == FALSE )
  {
	/* We need to wait for the started signal after releasing the mutex. */
    getStartedCurrentWorkSemaphore() . clearWakeupAlreadyReceived();

    getThreadBusyMutex().unlock("TaskExecutionThread:executeTask");

	/* Wake up the other thread -- it now has work. */
    getWorkSemaphore()
      . wakeupOtherThread ( Semaphore::THREAD_NOT_YET_WAITING_ALLOWED );

	/* Wait until other thread has locked itself into the busy state. */
    getStartedCurrentWorkSemaphore()
      . waitForSignal ( Semaphore::WAKEUP_ALREADY_HAPPENED_ALLOWED );

    getShouldEndMutex().unlock  ("TaskExecutionThread:executeTask");
  }

  return SUCCESS;
}


	/* Interface -- This is called by the Main Thread only! */
void
TaskExecutionThread::endThreadAfterCurrentTask()
{
  getShouldEndMutex().lock("TaskExecutionThread:endThreadAfterCurrentTask");

	/* Should-end flag must be set prior to wakeupOtherThread *
	 * to avoid a race condition.                             */
  shouldEndFlag = TRUE;

  getWorkSemaphore()
    . wakeupOtherThread ( Semaphore::THREAD_NOT_YET_WAITING_ALLOWED );

  getShouldEndMutex().unlock("TaskExecutionThread:endThreadAfterCurrentTask");
}






/***************************************************************************
 ***************************    ThreadManager    ***************************
 ***************************************************************************/


/*static*/ ThreadManager &
ThreadManager::getGlobalThreadManager()
{
  static ThreadManager * globalThreadManager = (ThreadManager *) NULL;

  if ( globalThreadManager == (ThreadManager *) NULL )
    globalThreadManager = new ThreadManager();
  return * globalThreadManager;
}




/*virtual*/
ThreadManager::~ThreadManager()
{
  setNumberOfThreads ( 0 );
}


BOOLEAN
ThreadManager::setNumberOfThreads ( unsigned int theNumberOfThreadsUnsigned )
{
  static int        currentlySettingThreads = -1;
  static Mutex mutexCurrentlySettingThreads ( Mutex::ERROR_CHECK );

  int  theNumberOfThreads = theNumberOfThreadsUnsigned;
  int4 i;

	/* Clamp the maximum number of threads.... */
  if ( theNumberOfThreads > ThreadManager::MAXIMUM_NUMBER_OF_THREADS )
    theNumberOfThreads = ThreadManager::MAXIMUM_NUMBER_OF_THREADS;


	/* setNumberOfThreads is *NOT* reentrant */
  mutexCurrentlySettingThreads.lock("ThreadManager:setNumberOfThreads");
  if ( currentlySettingThreads >= 0 )
  {
    tcmMessage ( "ThreadManager::setNumberOfThreads:  Failure: "
		 "Unable to run set %d threads.  "
		 "setNumberOfThreads already running to set %d threads, "
		 "and this routine is not reentrant!\n",
		 theNumberOfThreads, currentlySettingThreads );
    mutexCurrentlySettingThreads.unlock("ThreadManager:setNumberOfThreads");
    return FALSE;
  }
  else
  {
    currentlySettingThreads = theNumberOfThreads;
    mutexCurrentlySettingThreads.unlock("ThreadManager:setNumberOfThreads");
  }



	/* Probably extraneous, but better safe than sorry. */
  lockMasterMutex("ThreadManager:setNumberOfThreads");

	/* This would help... */
  getMutexThreadArray()    .lock("ThreadManager:setNumberOfThreads");
  getMutexNumberOfThreads().lock("ThreadManager:setNumberOfThreads");



	/* Trivial case. */
  if ( theNumberOfThreads == numberOfThreads )
  {
    getMutexNumberOfThreads().unlock("ThreadManager:setNumberOfThreads");
    getMutexThreadArray()    .unlock("ThreadManager:setNumberOfThreads");
    unlockMasterMutex               ("ThreadManager:setNumberOfThreads");

    mutexCurrentlySettingThreads.lock  ("ThreadManager:setNumberOfThreads");
    currentlySettingThreads = -1;
    mutexCurrentlySettingThreads.unlock("ThreadManager:setNumberOfThreads");

    return TRUE;
  }

	/* Add cases. */
  else if ( theNumberOfThreads > numberOfThreads )
  {
    TaskExecutionThread * * oldThreadArray     = threadArray;
    int                     oldNumberOfThreads = numberOfThreads;

    numberOfThreads = theNumberOfThreads;
    threadArray     = new TaskExecutionThread * [ theNumberOfThreads ];

    for ( i = 0;   i < oldNumberOfThreads;  i ++ )
      threadArray [ i ] = oldThreadArray [ i ];

    for ( ;   i < int4(theNumberOfThreads);   i++ )
    {
      threadArray [ i ] = new TaskExecutionThread();

	/* If we just hit a system limit on the number of threads... */
      if ( threadArray [ i ] -> initialize() != SUCCESS )
      {
	tcmMessage ( "ThreadManager::setNumberOfThreads:  Failure: "
		     "Unable to set %d threads.  "
		     "System limit reached at %d threads.\n",
		     numberOfThreads, i );
	delete threadArray [ i ];
	numberOfThreads = i;
	break;
      }
    }

    if ( oldThreadArray != (TaskExecutionThread * *) NULL )
      delete [] oldThreadArray;

    getMutexNumberOfThreads().unlock("ThreadManager:setNumberOfThreads");
    getMutexThreadArray()    .unlock("ThreadManager:setNumberOfThreads");
    unlockMasterMutex               ("ThreadManager:setNumberOfThreads");

    mutexCurrentlySettingThreads.lock  ("ThreadManager:setNumberOfThreads");
    currentlySettingThreads = -1;
    mutexCurrentlySettingThreads.unlock("ThreadManager:setNumberOfThreads");
    return TRUE;

  } /* else if ( theNumberOfThreads > numberOfThreads ) */


  else /* Subtract case. */
  {
	/* Someday we should sort out the "non-busy" threads,
	 * and delete them first...
	 */

	/* Stop/Delete the extra threads. */
    for ( i = numberOfThreads - 1;   i >= int4(theNumberOfThreads);   i-- )
    {
      TaskExecutionThread * currentTaskExecutionThread = threadArray [ i ];

      currentTaskExecutionThread -> endThreadAfterCurrentTask();

	/* Fully unlock ourselves. */
      getMutexNumberOfThreads().unlock("ThreadManager:setNumberOfThreads");
      getMutexThreadArray()    .unlock("ThreadManager:setNumberOfThreads");
      unsigned int numberOfLocks
	= TCM_FullyUnlockMasterMutexReturningNumberOfLocks();

      currentTaskExecutionThread -> waitForThreadToStop();

	/* Fully re-lock ourselves. */
      TCM_FullyRelockMasterMutexNumberOfLocksTimes ( numberOfLocks );
      getMutexThreadArray()    .lock("ThreadManager:setNumberOfThreads");
      getMutexNumberOfThreads().lock("ThreadManager:setNumberOfThreads");

      delete currentTaskExecutionThread;
      threadArray [ i ] = (TaskExecutionThread *) NULL;
    }

	/* We could re-allocate the thread array, but why?
	 * We will never reclaim all that much memory.  And the overhead
	 * in doing so will be much more expensive in and of itself.
	 * (Not to mention debugging & support.)
	 */

    numberOfThreads = theNumberOfThreads;

    if ( numberOfThreads <= 0 )
    {
      delete [] threadArray;
      threadArray = (TaskExecutionThread * *) NULL;
    }

    getMutexNumberOfThreads().unlock("ThreadManager:setNumberOfThreads");
    getMutexThreadArray()    .unlock("ThreadManager:setNumberOfThreads");
    unlockMasterMutex               ("ThreadManager:setNumberOfThreads");

    mutexCurrentlySettingThreads.lock  ("ThreadManager:setNumberOfThreads");
    currentlySettingThreads = -1;
    mutexCurrentlySettingThreads.unlock("ThreadManager:setNumberOfThreads");
    return TRUE;
  }

}/* void ThreadManager::setNumberOfThreads( unsigned int theNumberOfThreads )*/






void
ThreadManager::waitForAllThreadsToFinishWork()
{
/* Note: Possible race contion here, even though getNumberOfThreads()
 *       and getThread are now mutexed.
 */
  for ( int4 i=0;   i < getNumberOfThreads();   i++ )
  {
    getThread ( i )
      -> getFinishedCurrentWorkSemaphore()
      .  waitForSignal ( Semaphore::WAKEUP_ALREADY_HAPPENED_ALLOWED );
  }
}


BOOLEAN
ThreadManager::hasThreadWithUnfinishedWork()
{
  getInternalMutex().lock("ThreadManager:hasThreadWithUnfinishedWork");

  for ( int4 i=0;   i < getNumberOfThreads();   i++ )
  {
    if (   ( getThread ( i )
	       -> getFinishedCurrentWorkSemaphore()
	       .  getWakeupAlreadyReceived()
	    )
	== FALSE )
    {
      getInternalMutex().unlock("ThreadManager:hasThreadWithUnfinishedWork");
      return TRUE;
    }
  }

  getInternalMutex().unlock("ThreadManager:hasThreadWithUnfinishedWork");
  return FALSE;
}



BOOLEAN
ThreadManager::executeTask ( const Task_Tree_Ref & theTask )
{
  int4 i;

  lockMasterMutex("ThreadManager:executeTask");
  getInternalMutex().lock("ThreadManager:executeTask");

  for ( i=0;   i < getNumberOfThreads();   i++ )
  {
    if ( getThread ( i ) -> executeTask ( theTask,
					  FALSE /* NOT a Repop Scenario */ )
	 == SUCCESS )
    {
      getInternalMutex().unlock("ThreadManager:executeTask");
      unlockMasterMutex("ThreadManager:executeTask");
      return TRUE; /* Task is currently running. */
    }
  } /* for ( i=0;   i < getNumberOfThreads();   i++ ) */


	/* Try to dynamically create another (new) thread, if we can... */
  if ( getNumberOfThreads() < getMaximumNumberOfDynamiclyAllocatedThreads() )
  {
    int  oldNumberOfThreads = getNumberOfThreads();

    setNumberOfThreads ( oldNumberOfThreads + 1 );

    if ( getNumberOfThreads() > oldNumberOfThreads )
    {
      if ( getThread ( oldNumberOfThreads )
	     -> executeTask ( theTask,
			      FALSE /* NOT a Repop Scenario */ )
	   == SUCCESS )
      {
	getInternalMutex().unlock("ThreadManager:executeTask");
	unlockMasterMutex("ThreadManager:executeTask");
	return TRUE; /* Task is currently running. */
      }
    } /* if ( getNumberOfThreads() > oldNumberOfThreads ) */
  } /* IF ( we can dynamically create another (new) thread ) */


	/* Ok.  No threads available.  Lets push it onto the FIFO queue, *
	 * and let the next thread that becomes available grab it.       */
  getPendingThreadedTaskQueue() . push ( theTask );
  getInternalMutex().unlock("ThreadManager:executeTask");
  unlockMasterMutex("ThreadManager:executeTask");
  return FALSE;
}

	/* We want children-threads to be able to re-pop the *
	 * task queue automatically when they are done...    *
	 * This is *NEVER* called by the Main Thread!        */
BOOLEAN
ThreadManager::threadRePopTaskQueue (
				 TaskExecutionThread * theTaskExecutionThread )
{
  BOOLEAN threadHasNewTask;

  getInternalMutex().lock("ThreadManager:threadRePopTaskQueue");

  if ( getPendingThreadedTaskQueue() . empty() )
    threadHasNewTask = FALSE;
  else
  {
    Task_Tree_Ref taskRef = * ( getPendingThreadedTaskQueue() . front() );

    if ( theTaskExecutionThread -> executeTask ( taskRef,
						 TRUE /* Is RePop Scenario */ )
	 == SUCCESS )
    {
      getPendingThreadedTaskQueue() . pop();
      threadHasNewTask = TRUE;
    }
    else
    {
      cerr << "[ThreadManager:threadRePopTaskQueue]  Impossible Error:  "
	   << "Repop failed for task: " << (*taskRef) << endl;
      threadHasNewTask = FALSE;
    }
  } /* if ( getPendingThreadedTaskQueue() . empty() ) ... ELSE ... */

  getInternalMutex().unlock("ThreadManager:threadRePopTaskQueue");

  return threadHasNewTask;
}



 /* Keep track of the Primary Thread (ProcessAgenda/Agenda.clearQueues). */
status_t
ThreadManager::setPrimaryThread (
		BOOLEAN   theReportFailure,                     /*= TRUE   */
		status_t  theReturnValueIfAlreadyPrimaryThread, /*= SUCCESS*/
		BOOLEAN   theReportIfAlreadyPrimaryThread,      /*= TRUE   */
		BOOLEAN * theWasAlreadyPrimaryThread            /*= NULL   */ )
{
  status_t returnValue = FAILURE;

  lockMasterMutex("ThreadManager:setPrimaryThread");

	/* Default value is false.. */
  if ( theWasAlreadyPrimaryThread != (BOOLEAN *) NULL )
    (* theWasAlreadyPrimaryThread) = FALSE;


  if ( hasPrimaryThread() == FALSE )
  {
    hasPrimaryThreadFlag = TRUE;
    primaryThreadId      = Thread::getThreadId();
    returnValue = SUCCESS;
  }
  else
  {
    if ( getIsThisThePrimaryThread() == TRUE )
    {
      if ( theReportIfAlreadyPrimaryThread == TRUE )
	cout << "[ThreadManager:setPrimaryThread]  Warning:  This "
	     << "Thread had already set itself as our primary thread."
	     << endl;

      returnValue = theReturnValueIfAlreadyPrimaryThread;

      if ( theWasAlreadyPrimaryThread != (BOOLEAN *) NULL )
	(* theWasAlreadyPrimaryThread) = TRUE;
    }
    else
    {
      if ( theReportFailure == TRUE )
	cout << "[ThreadManager:setPrimaryThread]  Error:  Another "
	     << "Thread has already set itself as our primary thread.  "
	     << "Aborting..." << endl;
      returnValue = FAILURE;
    }
  }

  unlockMasterMutex("ThreadManager:setPrimaryThread");
  return returnValue;
}


status_t
ThreadManager::clearPrimaryThread()
{
  status_t returnValue = FAILURE;

  lockMasterMutex("ThreadManager:clearPrimaryThread");

  if ( getIsThisThePrimaryThread() == TRUE )
  {
    hasPrimaryThreadFlag = FALSE;
    Thread::clearThread ( & primaryThreadId );
    returnValue = SUCCESS;
  }
  else
  {
    if ( hasPrimaryThread() == FALSE )
    {
      cout << "[ThreadManager:clearPrimaryThread]  Warning:  "
	   << "There was NO Primary Thread set (to clear)." << endl;
      returnValue = SUCCESS;
    }
    else
    {
      cout << "[ThreadManager:clearPrimaryThread]  Error:  "
	   << "This is not the current Primary Thread.  Aborting..."
	   << endl;
      returnValue = FAILURE;
    }
  }

  unlockMasterMutex("ThreadManager:clearPrimaryThread");
  return returnValue;
}


Task_Tree_Ref
ThreadManager::getTaskTreeRefForThisThread()
{
  Task_Tree_Ref  returnValue;
  returnValue . clear();

	/* Probably extraneous, but this is a fast routine, and these will *
	 * keep us from changing anything out from underneath us...        */
  lockMasterMutex               ("ThreadManager:getTaskTreeRefForThisThread");
  getInternalMutex()       .lock("ThreadManager:getTaskTreeRefForThisThread");
  getMutexThreadArray()    .lock("ThreadManager:getTaskTreeRefForThisThread");
  getMutexNumberOfThreads().lock("ThreadManager:getTaskTreeRefForThisThread");

  for ( int4 i=0;   i < getNumberOfThreadsNoMutex();   i++ )
  {
    if ( Thread::equalsCurrent ( getThreadNoMutex ( i ) ) )
    {
      returnValue = getThreadNoMutex ( i ) -> getCurrentTaskRef();
      break;
    }
  }

 getMutexNumberOfThreads().unlock("ThreadManager:getTaskTreeRefForThisThread");
 getMutexThreadArray()    .unlock("ThreadManager:getTaskTreeRefForThisThread");
 getInternalMutex()       .unlock("ThreadManager:getTaskTreeRefForThisThread");
 unlockMasterMutex               ("ThreadManager:getTaskTreeRefForThisThread");

  return returnValue;
}

