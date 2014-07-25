
/*
 *  Semaphore.H -- simplistic pthread semaphore encapsulation.
 *
 *  Notes:  Used by two threads, so one thread can block until another
 *          thread tells it to continue.
 *
 *  Operation:
 *    Thread1  ----> waitForSignal()
 *    Thread2  ----> wakeupOtherThread()
 *    Thread2  <---- wakeupOtherThread() returns  --+ These two
 *    Thread1  <---- waitForSignal() returns      --+ are concurrent.
 *
 *  Or (non-concurrency situation):
 *    Thread1  ----> clearWakeupAlreadyReceived()
 *    Thread1  <---- clearWakeupAlreadyReceived() returns
 *
 *    Thread2  ----> wakeupOtherThread( TRUE )
 *    Thread2  <---- wakeupOtherThread( TRUE ) returns
 *
 *    Thread1  ----> waitForSignal ( TRUE )
 *    Thread1  <---- waitForSignal ( TRUE ) returns
 *
 *  Note:  wakeupAlreadyReceived is **NOT** cleared automatically
 *         in waitForSignal().  You must invoke clearWakeupAlreadyReceived()
 *         at the appropriate time.  Beware race conditions.
 *
 *  Note:  Use wakeupOtherThread(TRUE) if one is uncertain as to
 *         whether another thread is already waiting.  Do not do:
 *           if ( getIsThreadWaiting() == TRUE ) wakeupOtherThread();
 *         As this could lead to a race condition wherein a thread
 *         starts waiting after the getIsThreadWaiting() function.
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


#ifndef SEMAPHORE_H
#define SEMAPHORE_H

#include "Thread_Common.H"
#include "Mutex.H"
#include "MutexedObject.H"
#include <semaphore.h>

class Semaphore : public MutexedObject, public threadIostreamBase
{
public:
  enum CONSTANTS
  {
    WAKEUP_ALREADY_HAPPENED_ALLOWED = TRUE,
    WAKEUP_MUST_OCCUR_AFTER_THIS    = FALSE,
    REPORT_IF_NO_THREAD_NOW_WAITING = FALSE,
    THREAD_NOT_YET_WAITING_ALLOWED  = TRUE
  };

protected:
  BOOLEAN  isThreadWaiting;
  BOOLEAN  wakeupAlreadyReceived; /* I hate race conditions. */
  sem_t    semaphoreData;

protected:
  BOOLEAN  getIsThreadWaitingNoMutex() const { return isThreadWaiting; }
  void     setIsThreadWaitingNoMutex( BOOLEAN theIsThreadWaiting )
			       { isThreadWaiting = theIsThreadWaiting; }

  BOOLEAN  getWakeupAlreadyReceivedNoMutex() const
			       { return wakeupAlreadyReceived;  }
  void     setWakeupAlreadyReceivedNoMutex()
			       { wakeupAlreadyReceived = TRUE;  }
  void     clearWakeupAlreadyReceivedNoMutex()
			       { wakeupAlreadyReceived = FALSE; }

public:
  Semaphore();

  virtual ~Semaphore();

		/* Thread One calls this method to start blocking....        *
		 * IT does **NOT** invoke clearWakeupAlreadyReceived()       *
		 * after it wakes up!!! This feature is exploited by Thread. */
  status_t waitForSignal( BOOLEAN theWakeupCouldHaveAlreadyHappened = FALSE );

		/* Thread two calls this method, which wakes up thread one.
		 * This method does **NOT** block!
		 *
		 * Use wakeupOtherThread(TRUE) if one is uncertain as to
		 * whether another thread is already waiting.  Do not do:
		 *   if ( getIsThreadWaiting() == TRUE ) wakeupOtherThread();
		 * As this could lead to a race condition wherein a thread
		 * starts waiting after the getIsThreadWaiting() function.
		 *
		 * The argument name is complex.  But I wanted to be able
		 * to use TRUE/TRUE for waitForSignal()/wakeupOtherThread()
		 * in the case where waitForSignal() occurs prior to 
		 * the wakeupOtherThread().
		 *
		 * So, use FALSE to report when no other thread is present,
		 * and TRUE to prevent such reporting.
		 */
  status_t wakeupOtherThread( BOOLEAN theDoNotReportIfNoOtherThread = FALSE );


  void     clearWakeupAlreadyReceived()
	     {
	       lock("Semaphore:clearWakeupAlreadyReceived");
	       clearWakeupAlreadyReceivedNoMutex();
	       unlock("Semaphore:clearWakeupAlreadyReceived");
	     }

  BOOLEAN  getWakeupAlreadyReceived()
	     {
	       lock("Semaphore:getWakeupAlreadyReceived");
	       BOOLEAN returnValue = getWakeupAlreadyReceivedNoMutex();
	       unlock("Semaphore:getWakeupAlreadyReceived");
	       return returnValue;
	     }


  BOOLEAN  getIsThreadWaiting()
	     {
	       lock("Semaphore:isThreadWaiting");
	       BOOLEAN returnValue = getIsThreadWaitingNoMutex();
	       unlock("Semaphore:isThreadWaiting");
	       return returnValue;
	     }


  sem_t &  getSemaphoreDataNoMutex() { return semaphoreData; }


  ostream & printObjectNonConst ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString );

	/* threadIostreamBase class interface *
	 * Note:  Casts away const'ness!      */
  virtual ostream & printObject ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString ) const;


private:  /* These two should *NEVER* be invoked.   Lets make sure of that! */
  Semaphore ( const Semaphore & theSemaphore );
  Semaphore & operator= ( const Semaphore & theSemaphore );
};

#endif /* SEMAPHORE_H */
