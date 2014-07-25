
/*
 *  test_mutextiming.C -- test of mutex overhead.
 */

#include "Mutex.H"

BOOLEAN   globalIsTrivialTest = FALSE;


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



Mutex::Mutex ( Mutex::TYPE  theMutexType /* = Mutex::RECURSIVE */ )
  : threadIostreamBase (              ),
    mutexType          ( theMutexType )
{
  if ( globalIsTrivialTest == TRUE )
    return;

  int mutexAttributeKind;
  int result;

	/* It's not paranoia if the computer is really out to get you... */
  memset ( & getMutexAttributes(), 0, sizeof(pthread_mutexattr_t) );
  memset ( & getMutexData(),       0, sizeof(pthread_mutex_t)     );

	/* Create the Mutex Attributes */
  pthread_mutexattr_init ( & getMutexAttributes() );

	/* Decide how we are configuring it. */
  switch ( theMutexType )
  {
    case Mutex::FAST:
      mutexAttributeKind = PTHREAD_MUTEX_FAST_NP;
      break;
      
    case Mutex::RECURSIVE:
      mutexAttributeKind = PTHREAD_MUTEX_RECURSIVE_NP;
      break;

    case Mutex::ERROR_CHECK:
      mutexAttributeKind = PTHREAD_MUTEX_ERRORCHECK_NP;
      break;

    default:
      cerr << "[Mutex:Mutex]  Error:  Bad MutexType value:  "
	   << int4(theMutexType) << ".  Defaulting to RECURSIVE Mutex."
	   << endl;
      mutexType = Mutex::RECURSIVE;
      mutexAttributeKind = PTHREAD_MUTEX_RECURSIVE_NP;
  }


	/* Configure it appropriately.                  *
	 * PTHREAD_MUTEX_SETKIND is defined in Mutex.H. */
  result = PTHREAD_MUTEX_SETKIND ( & getMutexAttributes(),
				     mutexAttributeKind    );
  switch ( result )
  {
    case 0:  /* It worked */
      break;

    case EINVAL:
      cerr << "[Mutex:Mutex(Mutex::TYPE)]  Programmer Error:  "
	   << "pthread_mutexattr_setkind_np(., " << mutexAttributeKind
	   << ") failed with EINVAL.  Attempting to continue as FAST MUTEX."
	   << endl;
      mutexType = Mutex::FAST;
      break;

    default:  /* What the hell? */
      cerr << "[Mutex:Mutex(Mutex::TYPE)]  UNKNOWN Error:  "
	   << "pthread_mutexattr_setkind_np(., " << mutexAttributeKind
	   << ") failed with: " << result
	   << ".  Attempting to continue as FAST MUTEX."
	   << endl;
      mutexType = Mutex::FAST;
      break;
  }

	/* And finally, initialize the mutex itself... */
  pthread_mutex_init ( & getMutexData(),
		       (   (result == 0)
		         ? & getMutexAttributes()
		         : (const pthread_mutexattr_t *)NULL ) );
}



/*virtual*/
Mutex::~Mutex()
{
  if ( globalIsTrivialTest == TRUE )
    return;

  int result;

  pthread_mutexattr_destroy ( & getMutexAttributes() );

  result = pthread_mutex_destroy ( & getMutexData() );
  switch ( result )
  {
    case 0:  /* It worked */
      break;

    case EBUSY:
      cerr << "[Mutex:~Mutex]  Error:  (" << (void*)this << ") [EBUSY]  "
	   << "Unable to properly destroy currently lock()'ed Mutex!."
	   << endl;
      break;

    default:
      cerr << "[Mutex:~Mutex]  Error: (" << (void *)this
	   << ")  pthread_mutex_destroy(.) returned: "
	   << result << "." << endl;
      break;
  }
}



status_t
Mutex::lock()
{
  if ( globalIsTrivialTest == TRUE )
    return ::SUCCESS;

  int result = pthread_mutex_lock ( & getMutexData() );
  switch ( result )
  {
    case 0:  /* It worked */
      return ::SUCCESS;

    case EINVAL:
      cerr << "[Mutex:lock]  Error:  [EINVAL]  "
	   << "Unable to lock improperly initialized mutex."
	   << endl;
      break;

    case EDEADLK:  /* Mutex::ERROR_CHECK only */
      cerr << "[Mutex:lock]  Error:  [EDEADLK]  "
	   << "Unable to lock mutex:  Mutex already locked by calling thread."
	   << endl;
      break;

    default:
      cerr << "[Mutex:lock]  Error:  pthread_mutex_lock(.) returned: "
	   << result << "." << endl;
      break;
  }

  return ::FAILURE;
}


Mutex::TRY_LOCK_STATUS_T
Mutex::trylock()
{
  if ( globalIsTrivialTest == TRUE )
    return Mutex::SUCCESS;

  int result = pthread_mutex_trylock ( & getMutexData() );
  switch ( result )
  {
    case 0:  /* It worked */
      return Mutex::SUCCESS;

    case EINVAL:
      cerr << "[Mutex:trylock]  Error:  [EINVAL]  "
	   << "Unable to lock improperly initialized mutex."
	   << endl;
      break;

    case EBUSY:  /* Already locked */
      return Mutex::ALREADY_LOCKED;

    default:
      cerr << "[Mutex:lock]  Error:  pthread_mutex_trylock(.) returned: "
	   << result << "." << endl;
      break;
  }

  return Mutex::FAILURE;
}


status_t
Mutex::unlock()
{
  if ( globalIsTrivialTest == TRUE )
    return ::SUCCESS;

  int result = pthread_mutex_unlock ( & getMutexData() );
  switch ( result )
  {
    case 0:  /* It worked */
      return ::SUCCESS;

    case EINVAL:
      cerr << "[Mutex:unlock]  Error:  [EINVAL]  "
	   << "Unable to unlock improperly initialized mutex."
	   << endl;
      break;

    case EPERM:  /* Mutex::ERROR_CHECK only */
      cerr << "[Mutex:unlock]  Error:  [EPERM]  "
	   << "Unable to unlock Mutex:  Calling thread does not own the mutex."
	   << endl;
      break;

    default:
      cerr << "[Mutex:lock]  Error:  pthread_mutex_unlock(.) returned: "
	   << result << "." << endl;
      break;
  }

  return ::FAILURE;
}



BOOLEAN
Mutex::isLocked()
{
  if ( globalIsTrivialTest == TRUE )
    return TRUE;

  Mutex::TRY_LOCK_STATUS_T  tryLockStatus;
  switch ( tryLockStatus = trylock() )
  {
    case Mutex::SUCCESS:
	/* We just locked ourself.  If unlock fails... *
	 * Well, we are locked...  And probably hosed. */
      switch ( unlock() )
      {
	case ::SUCCESS:
	  return FALSE;

	    /* If this happens, something truly odd is going on... */
	case ::FAILURE:
	default:
	  cerr << "[Mutex:isLocked]  Internal Failure:  "
	       << "trylock() returned SUCCESS.   unlock() returned FAILURE.  "
	       << "(So we've just locked the mutex.)" << endl;
	  return TRUE;
      }

    case Mutex::FAILURE:
      return FALSE;

    case Mutex::ALREADY_LOCKED:
      return TRUE;

    default:
      cerr << "[Mutex:isLocked]  Impossible Error: tryLock() returned: "
	   << int4(tryLockStatus) << ".  Assuming we are locked..." << endl;
      return TRUE; /* Worst case scenario: Assume we are locked. */
  }
}




status_t
Mutex::lock ( const char * theErrorLocation )
{
  status_t returnValue = lock();

  if ( returnValue != ::SUCCESS )
  {
    cerr << "[" << theErrorLocation << "]  Error:  "
	 << "Unable to lock mutex." << endl;
  }

  return returnValue;
}

status_t
Mutex::unlock( const char * theErrorLocation)
{
  status_t returnValue = unlock();

  if ( returnValue != ::SUCCESS )
  {
    cerr << "[" << theErrorLocation << "]  Error:  "
	 << "Unable to unlock mutex." << endl;
  }

  return returnValue;
}




ostream &
Mutex::printObjectNonConst ( ostream &    theOstream,
			     u_int4       theIndent,
			     const char * thePrefixString )
{
  threadIostreamIndent  indent(theIndent, thePrefixString);

  theOstream << "Mutex:  " << (const void*)this

	     << "   ("
	     << ( isLocked() == TRUE ? "Locked" : "Unlocked" )
	     << ")"

	     << endl;

  return theOstream;
}

	/* threadIostreamBase class interface */
/*virtual*/ ostream &
Mutex::printObject ( ostream &    theOstream,
		     u_int4       theIndent,
		     const char * thePrefixString ) const
{
  return ((Mutex *) this)
	    -> printObjectNonConst ( theOstream, theIndent, thePrefixString );
}



inline struct timeval
getTime( struct timeval & theTime )
{
  if ( gettimeofday ( & theTime, (struct timezone *)NULL ) == -1 )
  {
    PERROR ( "[getTime] Error:  Bad value for theTime.  " );
    theTime.tv_sec  = 0;
    theTime.tv_usec = 0;
  }

  return theTime;
}

/*
 * "struct timeval" type.
 * Caveat Emptor:  This is *NOT* thread-safe!!!
 */
inline ostream &
operator << ( ostream & theOstream, const struct timeval & theTimeval )
{
  char        oldFillChar = theOstream.fill();
  struct tm * currentTime = localtime ( & theTimeval.tv_sec );
  

  if ( theTimeval.tv_sec > 86400 )
  {
    theOstream << setfill ( '0' )
	       << setw(4) << (currentTime->tm_year + 1900) << "."
	       << setw(2) << (currentTime->tm_mon  +    1) << "."
	       << setw(2) << currentTime->tm_mday          << "-"
	       << setw(2) << currentTime->tm_hour          << ":"
	       << setw(2) << currentTime->tm_min           << ":"
	       << setw(2) << currentTime->tm_sec           << "."
	       << setw(6) << theTimeval.tv_usec
	       << setfill ( oldFillChar );
  }
  else if ( theTimeval.tv_sec >= 3600 )
  {
    theOstream << setfill ( '0' )
	       << setw(2) << currentTime->tm_hour          << ":"
	       << setw(2) << currentTime->tm_min           << ":"
	       << setw(2) << currentTime->tm_sec           << "."
	       << setw(6) << theTimeval.tv_usec
	       << setfill ( oldFillChar );
  }

  else if ( theTimeval.tv_sec >= 60 )
  {
    theOstream << setfill ( '0' )
	       << setw(2) << currentTime->tm_min           << ":"
	       << setw(2) << currentTime->tm_sec           << "."
	       << setw(6) << theTimeval.tv_usec
	       << setfill ( oldFillChar );
  }

  else
  {
    theOstream << setfill ( '0' )
	       << setw(2) << currentTime->tm_sec           << "."
	       << setw(6) << theTimeval.tv_usec
	       << setfill ( oldFillChar );
  }

  return theOstream;
}


inline void
normalizeTime ( struct timeval & theTimeVal )
{
  if ( theTimeVal . tv_usec < 0 )
  {
    theTimeVal . tv_sec  += theTimeVal . tv_usec / 1000000;
    theTimeVal . tv_usec %= -1000000;
    theTimeVal . tv_sec --;
    theTimeVal . tv_usec += 1000000;
  }
  else if ( theTimeVal . tv_usec >= 1000000 )
  {
    theTimeVal . tv_sec  += theTimeVal . tv_usec / 1000000;
    theTimeVal . tv_usec %= 1000000;
  }
}

inline void
subtractTime ( struct timeval & theTimeVal, int4 theMillisecondsToSubtract )
{
  theTimeVal . tv_sec  -= theMillisecondsToSubtract / 1000000;
  theTimeVal . tv_usec -= theMillisecondsToSubtract % 1000000;
  normalizeTime ( theTimeVal );
}

inline void
addTime ( struct timeval & theTimeVal, int4 theMillisecondsToAdd )
{
  subtractTime ( theTimeVal, - theMillisecondsToAdd );
}


inline struct timeval
operator+ (       struct timeval   theFirstTimeVal,   /* Copy onto stack. */
            const struct timeval & theSecondTimeVal ) /* Reference!       */
{
  theFirstTimeVal . tv_sec  += theSecondTimeVal . tv_sec;
  theFirstTimeVal . tv_usec += theSecondTimeVal . tv_usec;
  normalizeTime ( theFirstTimeVal );
  return theFirstTimeVal;
}

inline struct timeval
operator- (       struct timeval   theFirstTimeVal,   /* Copy onto stack. */
            const struct timeval & theSecondTimeVal ) /* Reference!       */
{
  theFirstTimeVal . tv_sec  -= theSecondTimeVal . tv_sec;
  theFirstTimeVal . tv_usec -= theSecondTimeVal . tv_usec;
  normalizeTime ( theFirstTimeVal );
  return theFirstTimeVal;
}


inline struct timeval
operator+= (       struct timeval & theFirstTimeVal,   /* CHANGED!   */
             const struct timeval & theSecondTimeVal ) /* Reference! */
{
  theFirstTimeVal . tv_sec  += theSecondTimeVal . tv_sec;
  theFirstTimeVal . tv_usec += theSecondTimeVal . tv_usec;
  normalizeTime ( theFirstTimeVal );
  return theFirstTimeVal;
}

inline struct timeval
operator-= (       struct timeval & theFirstTimeVal,   /* CHANGED!   */
             const struct timeval & theSecondTimeVal ) /* Reference! */
{
  theFirstTimeVal . tv_sec  -= theSecondTimeVal . tv_sec;
  theFirstTimeVal . tv_usec -= theSecondTimeVal . tv_usec;
  normalizeTime ( theFirstTimeVal );
  return theFirstTimeVal;
}

inline int4
timeDifferenceInMilliseconds ( const struct timeval & theFirstTimeVal,
                               const struct timeval & theSecondTimeVal )
{
  return   ( (theFirstTimeVal.tv_sec  - theSecondTimeVal.tv_sec ) * 1000000 )
         + (  theFirstTimeVal.tv_usec - theSecondTimeVal.tv_usec            );
}






#define POUT(X)    cout << # X " = " << (X);
#define POUTNL(X)  cout << # X " = " << (X) << endl


		           /*1000*/
		        /*1000*/
#define CREATION_COUNT  100000000
#define LOCK_COUNT     1000000000

int main()
{
  Mutex           fastMutex      ( Mutex::FAST        );
  Mutex           recursiveMutex ( Mutex::RECURSIVE   );
  Mutex           errorMutex     ( Mutex::ERROR_CHECK );
  Mutex *         mutexPointer;
  int             i;
  struct timeval  startTime, endTime;

  POUTNL ( (globalIsTrivialTest = FALSE) );
  POUTNL ( CREATION_COUNT );


  getTime ( startTime );

  for ( i=0; i < CREATION_COUNT;  i++ )
  {
    mutexPointer = new Mutex(Mutex::FAST);
    delete mutexPointer;
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Creation test, non-trivial, fast.......:  " << endTime << endl;


  getTime ( startTime );

  for ( i=0; i < CREATION_COUNT;  i++ )
  {
    mutexPointer = new Mutex(Mutex::RECURSIVE);
    delete mutexPointer;
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Creation test, non-trivial, recursive..:  " << endTime << endl;



  getTime ( startTime );

  for ( i=0; i < CREATION_COUNT;  i++ )
  {
    mutexPointer = new Mutex(Mutex::ERROR_CHECK);
    delete mutexPointer;
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Creation test, non-trivial, error-check:  " << endTime << endl;



  POUTNL ( globalIsTrivialTest = TRUE );

  getTime ( startTime );

  for ( i=0; i < CREATION_COUNT;  i++ )
  {
    mutexPointer = new Mutex(Mutex::FAST);
    delete mutexPointer;
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Creation test, trivial.................:  " << endTime << endl;


///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////


  POUTNL ( globalIsTrivialTest = FALSE );

  getTime ( startTime );

  for ( i=0; i < LOCK_COUNT;  i++ )
  {
    fastMutex.lock();
    fastMutex.unlock();
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Lock test, non-trivial, fast.......:  " << endTime << endl;
  


  getTime ( startTime );

  for ( i=0; i < LOCK_COUNT;  i++ )
  {
    recursiveMutex.lock();
    recursiveMutex.unlock();
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Lock test, non-trivial, recursive..:  " << endTime << endl;
  


  getTime ( startTime );

  for ( i=0; i < LOCK_COUNT;  i++ )
  {
    errorMutex.lock();
    errorMutex.unlock();
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Lock test, non-trivial, error-check:  " << endTime << endl;
  

  POUTNL ( globalIsTrivialTest = TRUE );

  getTime ( startTime );

  for ( i=0; i < LOCK_COUNT;  i++ )
  {
    fastMutex.lock();
    fastMutex.unlock();
  }

  getTime ( endTime );
  endTime -= startTime;
  cout << "Lock test, trivial.................:  " << endTime << endl;

}
