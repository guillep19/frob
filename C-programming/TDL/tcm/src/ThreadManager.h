
/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 2001 Reid Simmons.  All rights reserved.
 *
 * FILE: ThreadManager.h
 *
 * ABSTRACT: The threading subsystem.
 *           Used only when THREADED is defined.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/ThreadManager.h,v $ 
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
 * $Log: ThreadManager.h,v $
 * Revision 1.3  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.2  2002/12/23 02:24:28  da0g
 * Assorted cleanup.  Added Mutexes to reduce race conditions.
 * Added mutexed getCurrentTaskRef()
 * Added getTaskTreeRefForThisThread()
 *
 * Revision 1.1  2001/10/23 22:52:57  da0g
 * Added Threading support.  Cached lastchild.
 *
 *
 *****************************************************************************/

#ifndef THREAD_MANAGER_H
#define THREAD_MANAGER_H

#include "tcmHandle.h"
#include "tcmPriv.h"
DECLARE_REF_COUNT_FUNCTIONS(Task_Tree_Node);
#include "queue.h"

#include "ThreadLib.H"

typedef queue<Task_Tree_Ref> Task_Queue;

class TaskExecutionThread : public Thread
{
protected:
  Semaphore        startedCurrentWorkSemaphore;
  Semaphore        finishedCurrentWorkSemaphore;
  Semaphore        workSemaphore; /* Wakeup: There's new work to do. */
  Mutex            threadBusyMutex;
  Mutex            shouldEndMutex;
  Mutex            currentTaskRefMutex;
  Task_Tree_Ref    currentTaskRef;
  BOOLEAN          shouldEndFlag;

public:
  TaskExecutionThread()
    : Thread                       ( Mutex::RECURSIVE        ),
      startedCurrentWorkSemaphore  (                         ),
      finishedCurrentWorkSemaphore (                         ),
      workSemaphore                (                         ),
      threadBusyMutex              ( Mutex::ERROR_CHECK      ),
      shouldEndMutex               ( Mutex::ERROR_CHECK      ),
      currentTaskRefMutex          ( Mutex::ERROR_CHECK      ),
      currentTaskRef               ( (Task_Tree_Node *) NULL ),
      shouldEndFlag                ( FALSE                   )
    { }

  status_t initialize()
    {
      if ( start() != SUCCESS )
	return FAILURE;
      
	/* Wait for thread main loop to be ready to accept new work. */
      getFinishedCurrentWorkSemaphore()
	.  waitForSignal ( Semaphore::WAKEUP_ALREADY_HAPPENED_ALLOWED );

      return SUCCESS;
    }


  virtual ~TaskExecutionThread();

  virtual void run(); /* The loop/code of this thread. */


  Semaphore     & getStartedCurrentWorkSemaphore()
			  { return startedCurrentWorkSemaphore;  }

  Semaphore     & getFinishedCurrentWorkSemaphore()
			  { return finishedCurrentWorkSemaphore; }

  Semaphore     & getWorkSemaphore()         { return workSemaphore;   }

  Mutex         & getThreadBusyMutex()       { return threadBusyMutex;} 
  Mutex         & getShouldEndMutex()        { return shouldEndMutex;  }
  Mutex         & getCurrentTaskRefMutex()   { return currentTaskRefMutex; }

  Task_Tree_Ref & getCurrentTaskRefNoMutex() { return currentTaskRef;  }
  Task_Tree_Ref   getCurrentTaskRef()
    { 
      getCurrentTaskRefMutex().lock("TaskExecutionThread:getCurrentTaskRef");
      Task_Tree_Ref returnTaskRef = getCurrentTaskRefNoMutex();
      getCurrentTaskRefMutex().unlock("TaskExecutionThread:getCurrentTaskRef");
      return returnTaskRef;
    }


  BOOLEAN getShouldEndNoMutex() const { return shouldEndFlag; }
  BOOLEAN getShouldEndMutexed()
    {
      getShouldEndMutex().lock  ("TaskExecutionThread:getShouldEndMutexed");
      BOOLEAN ourShouldEndFlagValue = getShouldEndNoMutex();
      getShouldEndMutex().unlock("TaskExecutionThread:getShouldEndMutexed");
      return ourShouldEndFlagValue;
    }



	/* Interface -- These are called by the Main Thread only! */
  status_t executeTask( const Task_Tree_Ref & theTaskRef,
			BOOLEAN               theIsRepopScenario = FALSE );
  void     endThreadAfterCurrentTask();

  BOOLEAN  getIsThreadWaitingForWork();
};

inline BOOLEAN
TaskExecutionThread::getIsThreadWaitingForWork()
{
  return TO_BOOLEAN (
	    /* Locked == thread is busy doing stuff. */
   (   ( getThreadBusyMutex().isLocked()               == FALSE )
	    /* wakeup == thread already has pending work. */
    && ( getWorkSemaphore().getWakeupAlreadyReceived() == FALSE ) ) );
}




/***************************************************************************
 ***************************    ThreadManager    ***************************
 ***************************************************************************/

class ThreadManager
{
public:
  static ThreadManager & getGlobalThreadManager();

  enum CONSTANTS
  {
    DEFAULT_NUMBER_OF_THREADS         = 0,

	/* Our default maximum number of threads to dynamaically allocate */
    DEFAULT_MAXIMUM_NUMBER_OF_THREADS = 100,

	/* Arbitrarily imposed limit for sanity checking... */
    MAXIMUM_NUMBER_OF_THREADS         = 100000 /* 100,000 */
  };


protected:
	/* Our internal mutex, to keep things sane... */
  Mutex                   internalMutex;

	/* The Master-Mutex for TCM, its count, and a mutex on that count. */
  Mutex                   masterMutex;
  Mutex                   masterCountMutex;
  int4                    masterCount;

	/* The Thread-array and number-of-threads mutexes. */
  Mutex                   mutexThreadArray;
  Mutex                   mutexNumberOfThreads;

	/* Array of pointers to threads.  Length >= numberOfThreads */
  TaskExecutionThread * * threadArray;

	/* Current Number of Threads. */
  int                     numberOfThreads;

	 /* Only used for dynamically allocating new threads upon demand...  *
	  * If set to zero, this disables dynamic allocation of new threads, *
	  * But does *NOT* affect or constrain setNumberOfThreads().         *
	  * Note: Its a maximum thread count.  Threads allocated with        *
	  * setNumberOfThreads() *ARE* *COUNTED* towards this limit.         */
  int                     maximumNumberOfDynamiclyAllocatedThreads;

	/* Tasks that don't have a thread available are queued up here,      *
	 * and automatically re-poped (run) when a thread becomes available, *
	 * in a first come, first served fashion.                            */
  Task_Queue              pendingThreadedTaskQueue;

	/* Used for ProcessAgenda(), aka: agenda.clearQueues(), to insure *
	 * that only one "primary" thread runs that method at a time.     */
  THREAD_ID               primaryThreadId;
  BOOLEAN                 hasPrimaryThreadFlag;



public:
  ThreadManager()
    : internalMutex            ( Mutex::ERROR_CHECK                ),
      masterMutex              ( Mutex::RECURSIVE                  ),
      masterCountMutex         ( Mutex::ERROR_CHECK                ),
      masterCount              ( 0                                 ),
      mutexThreadArray         ( Mutex::ERROR_CHECK                ),
      mutexNumberOfThreads     ( Mutex::ERROR_CHECK                ),
      threadArray              ( (TaskExecutionThread * *) NULL    ),
      numberOfThreads          ( 0                                 ),
      maximumNumberOfDynamiclyAllocatedThreads (
	          ThreadManager::DEFAULT_MAXIMUM_NUMBER_OF_THREADS ),
      pendingThreadedTaskQueue (                                   ),
      hasPrimaryThreadFlag     ( FALSE                             )
    {
      Thread::clearThread ( & primaryThreadId );
      setNumberOfThreads ( ThreadManager::DEFAULT_NUMBER_OF_THREADS );
    }

  virtual ~ThreadManager();

  BOOLEAN lockMasterMutex( const char * theErrorLocation = (const char *)NULL )
    {
      if ( masterMutex.lock(theErrorLocation) == SUCCESS )
      {
	  /* lock() is a blocking function call.
	   * Once we are locked, the same thread must unlock us.
	   * Therefore, this is safe to do here, in this thread.
	   */
	getMasterCountMutex().lock("ThreadManager:lockMasterMutex");
	masterCount++;
	getMasterCountMutex().unlock("ThreadManager:lockMasterMutex");
	return TRUE;
      }
      else
	return FALSE;
    }

  BOOLEAN unlockMasterMutex (
		      const char * theErrorLocation      = (const char *)NULL,
		      BOOLEAN      theMutexAlreadyLocked = FALSE              )

    {
      BOOLEAN returnValue;

	/* An unbelievably hairy race condition exists here, if we invoke
	 * lockMasterMutex() after the masterMutex.unlock() succeeds, but
	 * *BEFORE* the getMasterCountMutex().lock() and masterCount--.
	 */
      if ( theMutexAlreadyLocked == FALSE )
	getMasterCountMutex().lock("ThreadManager:unlockMasterMutex");

      if ( masterMutex.unlock(theErrorLocation) == SUCCESS )
      {
	masterCount--;
	if ( masterCount < 0 )
	{
	  cerr << "[ThreadManager:unlockMasterMutex]  Warning:  "
	       << "MasterMutex successfully unlocked more times than it "
	       << "was ever locked!  Confused.  Assuming zero locks now...  "
	       << "[Thread=" << Thread::getThreadId() << "]" << endl;
	  masterCount = 0;
	}
	returnValue = TRUE;
      }
      else
	returnValue = FALSE;

      if ( theMutexAlreadyLocked == FALSE )
	getMasterCountMutex().unlock("ThreadManager:unlockMasterMutex");

      return returnValue;
    }



  /* Sometimes, we need to temporarily remove all the current locks while
   * we wait for some event to transpire, task to exectute, etc.
   * These two functions accomplish this.
   */
  unsigned int fullyUnlockMasterMutexReturningNumberOfLocks (
			   const char * theErrorLocation = (const char *)NULL )
    {
      unsigned int i, numberOfLocks;

      getMasterCountMutex()
	. lock("ThreadManager:fullyUnlockMasterMutexReturningNumberOfLocks");

      numberOfLocks = masterCount;

      for ( i=numberOfLocks;  i > 0;  i-- )
      {
	unlockMasterMutex ( theErrorLocation, TRUE );
      }

      getMasterCountMutex()
	. unlock("ThreadManager:fullyUnlockMasterMutexReturningNumberOfLocks");

      return numberOfLocks;
    }

  void fullyRelockMasterMutexNumberOfLocksTimes (
			   unsigned int theNumberOfLocks,
			   const char * theErrorLocation = (const char *)NULL )
    {
      for ( ;   theNumberOfLocks > 0;   theNumberOfLocks -- )
	lockMasterMutex ( theErrorLocation );
    }



  Mutex & getMutexThreadArray()     { return mutexThreadArray; }
  Mutex & getMutexNumberOfThreads() { return mutexNumberOfThreads; }


  BOOLEAN setNumberOfThreads ( unsigned int theNumberOfThreadsUnsigned );
  int     getNumberOfThreadsNoMutex() { return numberOfThreads; }
  int     getNumberOfThreads()
    {
      getMutexNumberOfThreads().lock("ThreadManager:getNumberOfThreads");
      int returnValue = getNumberOfThreadsNoMutex();
      getMutexNumberOfThreads().unlock("ThreadManager:getNumberOfThreads");
      return returnValue;
    }



  int   getMaximumNumberOfDynamiclyAllocatedThreads()
    { return maximumNumberOfDynamiclyAllocatedThreads; }

  void  setMaximumNumberOfDynamiclyAllocatedThreads ( int theMaximumNumber )
    { maximumNumberOfDynamiclyAllocatedThreads = theMaximumNumber; }




  void waitForAllThreadsToFinishWork();

  BOOLEAN hasThreadWithUnfinishedWork();



	/* External Mechanism by which Threaded Tasks are Spawned. */
  BOOLEAN executeTask ( const Task_Tree_Ref & theTask );

	/* Internal Mechanism:  We want Children TaskExecutionThread's *
	 * to be able to re-pop the task queue automatically when they *
	 * are done...  This is *NEVER* called by the Primary Thread!  */
  BOOLEAN threadRePopTaskQueue ( TaskExecutionThread * theTaskExecutionThread);





    /* Keep track of the Primary Thread (ProcessAgenda/Agenda.clearQueues). */

    /* Note:  Using lockMasterMutex/unlockMasterMutex as insurance --
     * lockMasterMutex/unlockMasterMutex should have been invoked prior to
     * any of these methods anyway, with the current TCM implementation.
     */
  BOOLEAN hasPrimaryThread()
    {
      lockMasterMutex("ThreadManager:hasPrimaryThread");
      BOOLEAN returnValue = hasPrimaryThreadFlag;
      unlockMasterMutex("ThreadManager:hasPrimaryThread");
      return returnValue;
    }


  THREAD_ID getPrimaryThreadId()
    {
      lockMasterMutex("ThreadManager:getPrimaryThreadId");
      THREAD_ID returnValue = primaryThreadId;
      unlockMasterMutex("ThreadManager:getPrimaryThreadId");
      return returnValue;
    }

  BOOLEAN getIsThisThePrimaryThread()
    {
      lockMasterMutex("ThreadManager:getIsThisThePrimaryThread");
      BOOLEAN returnValue
	= TO_BOOLEAN (   ( hasPrimaryThreadFlag  == TRUE            )
		      && ( Thread::equals ( Thread::getThreadId(),
					    primaryThreadId       ) ) );
      unlockMasterMutex("ThreadManager:getIsThisThePrimaryThread");
      return returnValue;
    }

  status_t setPrimaryThread (
	    BOOLEAN   theReportFailure                     = TRUE,
	    status_t  theReturnValueIfAlreadyPrimaryThread = SUCCESS,
	    BOOLEAN   theReportIfAlreadyPrimaryThread      = TRUE,
	    BOOLEAN * theWasAlreadyPrimaryThread           = (BOOLEAN *)NULL );

  status_t clearPrimaryThread();

  Task_Tree_Ref getTaskTreeRefForThisThread();


private:
  Mutex      & getMasterCountMutex()         {return masterCountMutex;        }
  Mutex      & getInternalMutex()            {return internalMutex;           }
  Task_Queue & getPendingThreadedTaskQueue() {return pendingThreadedTaskQueue;}

  TaskExecutionThread * * getThreadArrayNoMutex() { return threadArray; }
  TaskExecutionThread *   getThreadNoMutex ( u_int4 theIndex )
    {
      if ( theIndex >= u_int4(getNumberOfThreads()) )
      {
	cerr << "[ThreadManager:getThread]  Error:  Bad thread index: "
	     << theIndex << endl;
	return (TaskExecutionThread *) NULL;
      }
      else
	return getThreadArrayNoMutex() [ theIndex ];
    }

  TaskExecutionThread *   getThread ( u_int4 theIndex )
    {
      getMutexThreadArray().lock("ThreadManager:getThread");
      TaskExecutionThread * returnValue = getThreadNoMutex ( theIndex );
      getMutexThreadArray().unlock("ThreadManager:getThread");
      return returnValue;
    }

};

#endif /* THREAD_MANAGER_H */
