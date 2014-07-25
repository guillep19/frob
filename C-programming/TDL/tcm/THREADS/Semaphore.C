
/*
 *  Semaphore.C -- simplistic pthread semaphore encapsulation.
 *
 *  Notes:  Used by two threads, so one thread can block until another
 *          thread tells it to continue.
 */

static const char * copyrightAndLicense
	= "Copyright (c) 2001, by David B. Apfelbaum.\n"
	  "All rights reserved.\n"
	  "\n"
	  "Standard BSD Open-Source License:\n"
	  "\n"
	  "Redistribution and use in source and binary forms,\n"
	  "with or without modification, are permitted provided\n"
	  "that the following conditions are met:\n"
	  "\n"
	  " + Redistributions of source code must retain the\n"
	  "   above copyright notice, this list of conditions\n"
	  "   and the following disclaimer.\n"
	  "\n"
	  " + Redistributions in binary form must reproduce the\n"
	  "   above copyright notice, this list of conditions\n"
	  "   and the following disclaimer in the documentation\n"
	  "   and/or other materials provided with the\n"
	  "   distribution. \n"
	  "\n"
	  " + Neither the name of David B. Apfelbaum nor the\n"
	  "   names of any contributors may be used to endorse\n"
	  "   or promote products derived from this software\n"
	  "   without specific prior written permission. \n"
	  "\n"
	  "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS\n"
	  "AND CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED\n"
	  "WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"
	  "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n"
	  "A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT\n"
	  "SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY\n"
	  "DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"
	  "CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,\n"
	  "PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF\n"
	  "USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n"
	  "HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,\n"
	  "WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"
	  "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY\n"
	  "WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED\n"
	  "OF THE POSSIBILITY OF SUCH DAMAGE.\n";
static void copyrightAndLicense_markUsed()
	{ (void)&copyrightAndLicense_markUsed; (void)&copyrightAndLicense; }



#include "Semaphore.H"

Semaphore::Semaphore()
  : MutexedObject         ( Mutex::RECURSIVE ),
    threadIostreamBase    (                  ),
    isThreadWaiting       ( FALSE            ),
    wakeupAlreadyReceived ( FALSE            )
{
  if ( sem_init ( & getSemaphoreDataNoMutex(),
		  0, /* Local to current process. (Only supported option)*/
		  0  /* Initial value */ )
       != 0 )
  {
    PERROR ( "[Semaphore:Semaphore]  Error:  "
	     "Unable to intialize semaphore-data.   " );
	/* Now what do we do?  Throw or exit? */
    throw "[Semaphore:Semaphore] Error:  sem_init failed";
  }
}


/*virtual*/
Semaphore::~Semaphore()
{
  if ( sem_destroy ( & getSemaphoreDataNoMutex() ) != 0 )
  {
    PERROR ( "Semaphore:Semaphore]  Error:  "
	     "Unable to destroy semaphore-data.  "
	     "(Some thread is probably still waiting on this semaphore.)   " );
  }
}


    /* Thread One calls this method to start blocking....        *
     * IT does **NOT** invoke clearWakeupAlreadyReceived()       *
     * after it wakes up!!! This feature is exploited by Thread. */
status_t
Semaphore::waitForSignal(BOOLEAN theWakeupCouldHaveAlreadyHappened /*= FALSE*/)
{
  lock("Semaphore::waitForSignal");

  if ( getIsThreadWaitingNoMutex() == TRUE )
  {
    cerr << "[Semaphore:waitForSignal]  Error:  "
	 << "Another thread is already waiting on this Semaphore." << endl;
    unlock("Semaphore::waitForSignal");    
    return FAILURE;
  }

	/* Has the wakeup call already been made? */
  if ( theWakeupCouldHaveAlreadyHappened == TRUE )
  {
    if ( getWakeupAlreadyReceivedNoMutex() == TRUE )
    {
      setIsThreadWaitingNoMutex ( FALSE );

	/* It is possible for a race condition to trigger a semaphore
	 * multiple times while the sem_wait is running...
	 * So lets detect that, and clean it up nicely...
	 */
      while ( sem_trywait ( & getSemaphoreDataNoMutex() ) == 0 )
      {
	cerr << endl << "[Semaphore:waitForSignal(waited)]  Warning:  "
	     << "Semaphore count was non-zero.  Decrementing..."
	     << endl << endl;
      }
      unlock("Semaphore::waitForSignal");    
      return SUCCESS;
    }
  }

  setIsThreadWaitingNoMutex ( TRUE );

  unlock("Semaphore::waitForSignal");

	/* sem_wait always returns 0 */
  sem_wait ( & getSemaphoreDataNoMutex() );

	/* It is possible for a race condition to trigger a semaphore
	 * multiple times while the sem_wait is running...
	 * So lets detect that, and clean it up nicely...
	 */
  while ( sem_trywait ( & getSemaphoreDataNoMutex() ) == 0 )
  {
    cerr << endl << "[Semaphore:waitForSignal(waiting)]  Warning:  "
	 << "Semaphore count was non-zero.  Decrementing..."
	 << endl << endl;
  }

	/* There is a rare race condtion wherein we resume running right
	 * after the sem_post(), and before the other thread finishes
	 * wakeupOtherThread(). This doesn't guarantee that wakeupOtherThread()
	 * has finished, but it improves the odds, and insures that our Mutex
	 * is unlocked when we return.
	 * Discovered through race condition wherein main() exited right after
	 * Thread::waitForThreadToStop() returned.
	 */
  lock("Semaphore::waitForSignal[2]");  
	/* As long as we are locked, this needs to be cleared... */
  setIsThreadWaitingNoMutex ( FALSE );
  unlock("Semaphore::waitForSignal[2]");

  return SUCCESS;
}



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
status_t
Semaphore::wakeupOtherThread( BOOLEAN theDoNotReportIfNoOtherThread /*=FALSE*/)
{
  lock("Semaphore:wakeupOtherThread");

  setWakeupAlreadyReceivedNoMutex(); /* Race conditions are a bitch! */

  if ( getIsThreadWaitingNoMutex() == FALSE )
  {
    if ( theDoNotReportIfNoOtherThread == FALSE )
    {
      FLUSHALL();
      cerr << "[Semaphore:waitForSignal]  Warning:  "
	   << "No thread is waiting on this Semaphore." << endl;
    }
    unlock("Semaphore::waitForSignal");    
    return FAILURE;
  }

	/* Only doing sem_post if we have a thread waiting... */
  if ( sem_post ( & getSemaphoreDataNoMutex() ) != 0 )
  {
    PERROR ( "[Semaphore:waitForSignal]  Error:  sem_post() failed.   " );
    unlock("Semaphore:wakeupOtherThread");
    return FAILURE;
  }

  unlock("Semaphore:wakeupOtherThread");
  return SUCCESS;
}



ostream &
Semaphore::printObjectNonConst ( ostream &    theOstream,
				 u_int4       theIndent,
				 const char * thePrefixString )
{
  int                   semGetValueResult;
  threadIostreamIndent  indent(theIndent, thePrefixString);

	/* sem_getvalue() always returns 0 */
  sem_getvalue ( & getSemaphoreDataNoMutex(),
		 & semGetValueResult );

  theOstream << "Semaphore:  " << (const void*)this << endl

	     << indent
	     << "   isThreadWaiting.......: " << getIsThreadWaiting()
	     << endl

	     << indent
	     << "   wakeupAlreadyReceived.: " << getWakeupAlreadyReceived()
	     << endl

	     << indent
	     << "   sem_getvalue..........:  " << semGetValueResult
	     << endl

	     << indent
	     << "   SemaphoreMutex: " << getMutex();

  return theOstream;
}

	/* threadIostreamBase class interface *
	 * Note:  Casts away const'ness!      */
/*virtual*/ ostream &
Semaphore::printObject ( ostream &    theOstream,
			 u_int4       theIndent,
			 const char * thePrefixString ) const
{
  return ((Semaphore *)this)
	    -> printObjectNonConst ( theOstream, theIndent, thePrefixString );
}


