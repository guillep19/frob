
/*
 *  Thread.H -- pthread encapsulation.
 *
 *  Notes:
 *    * Using default (NULL) thread attributes parameters.
 *    * Detaches created threads to avoid memory leaks.
 *      (Resources are free'd upon thread termination,
 *       at the expense of being unable to perform pthread_join.)
 */

/* 
 * Copyright (c) 2001, by David B. Apfelbaum.
 * All rights reserved.
 *
 * Standard BSD Open-Source License:
 *
 * Redistribution and use in source and binary forms,
 * with or without modification, are permitted provided
 * that the following conditions are met:
 *
 *  + Redistributions of source code must retain the
 *    above copyright notice, this list of conditions
 *    and the following disclaimer. 
 *
 *  + Redistributions in binary form must reproduce the
 *    above copyright notice, this list of conditions
 *    and the following disclaimer in the documentation
 *    and/or other materials provided with the
 *    distribution. 
 *
 *  + Neither the name of David B. Apfelbaum nor the
 *    names of any contributors may be used to endorse
 *    or promote products derived from this software
 *    without specific prior written permission. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS
 * AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 * WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#ifndef THREAD_H
#define THREAD_H

#include "Thread_Common.H"
#include <pthread.h>
#include <sched.h>
#include "Mutex.H"
#include "MutexedObject.H"
#include "Semaphore.H"

typedef pthread_t  THREAD_ID;
#define PRINTF_THREAD_ID_STRING            "%lu"
#define PRINTF_THREAD_ID(THREAD_ID_VALUE)  (THREAD_ID_VALUE)

typedef void ( * pointerToVoidFunction ) ( void );


class Thread : public MutexedObject, public threadIostreamBase
{
public:
  enum STATUS
  {
    CREATED  = 0,
    RUNNING  = 1,
    FINISHED = 2,
    DELETED  = 3
  };

  enum CONSTANTS
  {
    CPU_TICK_IN_MICROSECONDS = 10000,
    YIELD_TIME               =  9999, /* CPU_TICK_IN_MICROSECONDS - 1 */
  };

	/* Class methods. */
public:
  static THREAD_ID getThreadId()
    { return pthread_self(); }

	/* This breaks badly if pthread_t is a class with virtual methods */
  static void clearThread ( THREAD_ID * theThreadId )
    { memset ( (void *) theThreadId,  0,  sizeof(THREAD_ID) ); }



  static int equals ( THREAD_ID theFirstThread, THREAD_ID theSecondThread )
    { return  pthread_equal ( theFirstThread, theSecondThread ); }

  static int equals ( THREAD_ID theFirstThread, Thread & theSecondThread)
    { return  theSecondThread . equals ( theFirstThread ); }

  static int equals ( THREAD_ID theFirstThread, Thread * theSecondThread)
    { return ( theSecondThread != (Thread *) NULL )
	? theSecondThread -> equals ( theFirstThread ) : FALSE; }

  static int equals ( Thread & theFirstThread, THREAD_ID theSecondThread)
    { return  theFirstThread . equals ( theSecondThread ); }

  static int equals ( Thread & theFirstThread, Thread & theSecondThread )
    { return  theFirstThread . equals ( theSecondThread ); }

  static int equals ( Thread & theFirstThread, Thread * theSecondThread )
    { return  theFirstThread . equals ( theSecondThread ); }

  static int equals ( Thread * theFirstThread, THREAD_ID theSecondThread)
    { return ( theFirstThread != (Thread *) NULL )
	? theFirstThread -> equals ( theSecondThread ) : FALSE; }

  static int equals ( Thread * theFirstThread, Thread & theSecondThread )
    { return ( theFirstThread != (Thread *) NULL )
	? theFirstThread -> equals ( theSecondThread ) : FALSE; }

  static int equals ( Thread * theFirstThread, Thread * theSecondThread )
    { return ( theFirstThread != (Thread *) NULL )
	? theFirstThread -> equals ( theSecondThread ) : FALSE; }

  static int equalsCurrent ( THREAD_ID theFirstThread )
    { return  Thread::equals ( theFirstThread, Thread::getThreadId() ); }

  static int equalsCurrent ( Thread & theFirstThread  )
    { return  Thread::equals ( theFirstThread, Thread::getThreadId() ); }

  static int equalsCurrent ( Thread * theFirstThread  )
    { return  Thread::equals ( theFirstThread, Thread::getThreadId() ); }




	/* This can be sched_yield().  However that is not guaranteed
	 * to permit another process to run.  (Specifically when this is the
	 * only process at the highest priority list.)
	 *
	 * usleep() for just under one one clock tick usually has better luck.
	 */
  static void yield() { usleep ( Thread::YIELD_TIME ); } /* Or sched_yield() */


  static const char * statusToString ( Thread::STATUS theStatus )
  {
    const char * strings[4] = { "CREATED", "RUNNING", "FINISHED", "DELETED" };
    if ( (theStatus >= Thread::CREATED)  &&  (theStatus <= Thread::DELETED) )
      return strings [ theStatus ];
    else
      return "**UNKNOWN**";
  }

	/* Used when launching thread, to run ourself. */
  static void * staticThreadRunFunction ( void * theData )
  {
    Thread * theThread = (Thread *)theData;
    theThread -> setThreadStatus ( Thread::RUNNING );
    theThread -> threadStarted();
    theThread -> run();
    theThread -> setThreadStatus ( Thread::FINISHED );
    theThread -> threadStopped();
    return (void *)NULL;
  }


private:
  Thread::STATUS        threadStatus;
  pthread_t             threadData;
  pointerToVoidFunction threadFunction;
  BOOLEAN               threadStopRequestFlag;
  Semaphore             threadStartedSemaphore;
  Semaphore             threadStoppedSemaphore;

public:
  Thread ( Mutex::TYPE  theMutexType = Mutex::RECURSIVE )
    : MutexedObject          ( theMutexType                   ),
      threadIostreamBase     (                                ),
      threadStatus           ( Thread::CREATED                ),
      threadFunction         ( pointerToVoidFunction ( NULL ) ),
      threadStopRequestFlag  ( FALSE                          ),
      threadStartedSemaphore (                                ),
      threadStoppedSemaphore (                                )
    { memset ( & threadData, 0, sizeof(threadData) ); }

  virtual ~Thread();

  status_t start();  /* Starts the thread */

  virtual void run(); /* The main loop of the thread. */

  Thread::STATUS  getThreadStatus()
    {
      lock("Thread:getThreadStatusMutexed");
      Thread::STATUS  ourThreadStatus = getThreadStatusNoMutex();
      unlock("Thread:getThreadStatusMutexed");
      return ourThreadStatus;
    }

  const char *  getThreadStatusString()
    { return Thread::statusToString ( getThreadStatus() ); }

	/* Note:  NOT MUTEXED!!! */
  pthread_t &  getThreadDataNoMutex() { return threadData; }


  pointerToVoidFunction  getThreadFunction()
    {
      lock("Thread:getThreadFunctionMutexed");
      pointerToVoidFunction ourThreadFunction = threadFunction;
      unlock("Thread:getThreadFunctionMutexed");
      return ourThreadFunction;
    }

  status_t  setThreadFunction( pointerToVoidFunction theThreadFunction )
    {
      lock("Thread:setThreadFunction");
      if ( getThreadStatusNoMutex() == Thread::CREATED )
      {
	threadFunction = theThreadFunction;
	unlock("Thread:setThreadFunction");
	return SUCCESS;
      }
      else
      {
	cerr << "[Thread:setThreadFunctionMutexed]  Error:  Can not set "
	     << "thread function after Thread has started Running.  status=\""
	     << getThreadStatusStringNoMutex()   << "\" ("
	     << int4(getThreadStatusNoMutex()) << ")." << endl;
	unlock("Thread:setThreadFunction");
	return FAILURE;
      }
    }


	/* Sometimes, a thread wants to run until it is told to stop.        *
	 * To accommodate this, that thread can poll getStopThreadRequested()*/
  BOOLEAN getStopThreadRequested()
  {
    lock("Thread:getStopThreadRequested");
    BOOLEAN ourThreadStopRequestFlag = threadStopRequestFlag;
    unlock("Thread:getStopThreadRequested");
    return ourThreadStopRequestFlag;
  }

  virtual void setStopThreadRequested()
  {
    lock("Thread:setStopThreadRequested");
    threadStopRequestFlag = TRUE;
    unlock("Thread:setStopThreadRequested");
  }

  void pollWaitForThreadToStart ( u_int4 theMicrosecondsToSleep = 10000 )
  {
    while ( getThreadStatus() == Thread::CREATED )
      usleep ( theMicrosecondsToSleep );
  }

  void pollWaitForThreadToStop ( u_int4 theMicrosecondsToSleep = 10000 )
  {
    pollWaitForThreadToStart ( theMicrosecondsToSleep );
    while ( getThreadStatus() == Thread::RUNNING )
      usleep ( theMicrosecondsToSleep );
  }

  status_t waitForThreadToStart()
  {
    return getThreadStartedSemaphore()
      . waitForSignal ( TRUE /* Could have already happenned */ );
  }

  status_t waitForThreadToStop()
  {
    return getThreadStoppedSemaphore()
      . waitForSignal ( TRUE /* Could have already happenned */ );
  }


  int equals ( THREAD_ID theThreadId )
  {
    lock("Thread:equalsThread(THREAD_ID)");
    int returnValue = pthread_equal ( getThreadDataNoMutex(), theThreadId );
    unlock("Thread:equalsThread(THREAD_ID)");
    return returnValue;
  }

  int equals ( Thread & theOtherThread )
  {
    lock("Thread:equalsThread");
    theOtherThread.lock("Thread:equalsThread");
    int returnValue = pthread_equal (                 getThreadDataNoMutex(),
				     theOtherThread . getThreadDataNoMutex() );
    theOtherThread.unlock("Thread:equalsThread");
    unlock("Thread:equalsThread");
    return returnValue;
  }

  int equals ( Thread * theOtherThread )
  {
    if ( theOtherThread != (Thread *) NULL )
      return equals ( * theOtherThread );
    else
      return FALSE;
  }

  int equals ( )
  {
    return equals ( Thread::getThreadId() );
  }


  ostream & printObjectNonConst ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString );

	/* threadIostreamBase class interface *
	 * Note:  Casts away const'ness!      */
  virtual ostream & printObject ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString ) const;


private:
  Thread::STATUS  getThreadStatusNoMutex() { return threadStatus; }

  const char *  getThreadStatusStringNoMutex()
    { return Thread::statusToString ( getThreadStatusNoMutex() ); }

  void setThreadStatus ( Thread::STATUS theThreadStatus )
    {
      lock("Thread:setThreadStatus");
      threadStatus = theThreadStatus;
      unlock("Thread:setThreadStatus");
    }

  Semaphore & getThreadStartedSemaphore() { return threadStartedSemaphore; }
  Semaphore & getThreadStoppedSemaphore() { return threadStoppedSemaphore; }

  status_t threadStarted()
    {
      return getThreadStartedSemaphore()
	. wakeupOtherThread( TRUE /* Don't report if no other thread yet */ );
    }

  status_t threadStopped()
    {
      return getThreadStoppedSemaphore()
	. wakeupOtherThread( TRUE /* Don't report if no other thread yet */ );
    }


private:  /* These two should *NEVER* be invoked.   Lets make sure of that! */
  Thread ( const Thread & theThread );
  Thread & operator= ( const Thread & theThread );
};

#endif /* THREAD_H */

