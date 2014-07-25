
/*
 *  Thread.C -- pthread encapsulation.
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



#include "Thread.H"


/*virtual*/
Thread::~Thread()
{
	/* In the off-chance memory is accessed after we are deleted... */
  setThreadStatus ( Thread::DELETED );
}


status_t
Thread::start()
{
  int  result;

  lock("Thread:start");

	/* Check for idiocy. */
  if ( getThreadStatusNoMutex() != Thread::CREATED )
  {
    cerr << "[Thread::start]  Error:  Bad Thread state.  status=\""
	     << getThreadStatusStringNoMutex() << "\" ("
	     << int4(getThreadStatusNoMutex()) << ")." << endl;
    unlock("Thread:start");
    return ::FAILURE;
  }

	/* Create the thread. */
  result = pthread_create ( & getThreadDataNoMutex(),
			    (pthread_attr_t *) NULL,
			    & Thread::staticThreadRunFunction,
			    (void *) ((Thread *) this) );
  switch ( result )
  {
    case 0:  /* It worked */
      break;

    case EAGAIN:
      cerr << "[Thread:start]  Create Error:  [EAGAIN]  "
	   << "Insufficient system resources to create new thread.  "
	   << "Possibly more than PTHREAD_THREADS_MAX threads already active."
	   << endl;
      unlock("Thread:start");
      return ::FAILURE;

    default:
      cerr << "[Thread::start]  Create Error:  pthread_create returns: "
	   << result << endl;
      unlock("Thread:start");
      return ::FAILURE;
  }

	/* Deatach the thread to avoid memory leaks. */
  result = pthread_detach ( getThreadDataNoMutex() );
  switch ( result )
  {
    case 0:  /* It worked */
      break;

    case ESRCH:
      cerr << "[Thread:start]  Detach Warning:  [ESRCH]  "
	   << "Unable to find newly created thread." << endl;
      break;  /* Return success anyway since pthread_create(.) worked. */
      
    case EINVAL:
      cerr << "[Thread:start]  Detach Warning:  [EINVAL]  "
	   << "Thread was already detached." << endl;
      break;  /* This counts as a success. */

    default:
      cerr << "[Thread:start]  Detach Warning:  pthread_detach(.) returns: "
	   << result << endl;
      break;  /* Return success anyway since pthread_create(.) worked. */
  }

  unlock("Thread:start");
  return ::SUCCESS;
}



/*virtual*/ void
Thread::run()
{
  pointerToVoidFunction  functionToRun = getThreadFunction();

  if ( functionToRun != pointerToVoidFunction(NULL) )
  {
    (* functionToRun)();
  }
  else
  {
    cerr << "[Thread:run]  Error: Tried to RUN Thread with a NULL "
	 << "Thread-Function." << endl;
  }
}




ostream &
Thread::printObjectNonConst ( ostream &    theOstream,
			      u_int4       theIndent,
			      const char * thePrefixString )
{
  threadIostreamIndent  indent(theIndent, thePrefixString);

  theOstream << "Thread:  " << (const void*)this << endl

	     << indent
	     << "   Status   = \""
	     << getThreadStatusString() << "\" ("
	     << int4(getThreadStatus()) << ")" << endl

	     << indent
	     << "   Function = " << (void*)getThreadFunction() << endl

	     << indent
	     << "   ThreadMutex: " << getMutex() << endl

	     << indent
	     << "   ThreadStartedSemaphore: ";

  getThreadStartedSemaphore() . printObject ( theOstream,
					      theIndent + 5,
					      thePrefixString );

  theOstream << endl
	     << indent
	     << "   ThreadStoppedSemaphore: ";

  getThreadStoppedSemaphore() . printObject ( theOstream,
					      theIndent + 5,
					      thePrefixString );
  theOstream << endl;

  return theOstream;
}


	/* threadIostreamBase class interface */
/*virtual*/ ostream &
Thread::printObject ( ostream &    theOstream,
		      u_int4       theIndent,
		      const char * thePrefixString ) const
{
  return ((Thread *) this)
	    -> printObjectNonConst ( theOstream, theIndent, thePrefixString );
}
