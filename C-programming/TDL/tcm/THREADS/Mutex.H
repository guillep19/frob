
/*
 *  Mutex.H -- pthread mutex encapsulation.
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


#ifndef MUTEX_H
#define MUTEX_H

	/* Red Hat 6.2 makes the set-mutex-type function difficult to access.
	 * We need to utilize _XOPEN_SOURCE in features to get what we need.
	 */
#ifndef _GNU_SOURCE
#define _XOPEN_SOURCE 500
#endif
#include <features.h>

#include "Thread_Common.H"


	/* RH 7.0 / 7.1 uses ADAPTIVE instead of FAST in "pthread.h".
	 * Defining __USE_GNU enables a backward-compatibility mode for FAST,
	 * and gives us access to a few other things as well.
	 */
#ifndef __USE_GNU
#define __USE_GNU
#define NEED_TO_UNDEF__USE_GNU
#endif

#include <pthread.h>

#ifdef NEED_TO_UNDEF__USE_GNU
#undef  __USE_GNU
#undef NEED_TO_UNDEF__USE_GNU
#endif


	/* In between Red Hat 5.2 and Red Hat 6.2, the invocation format
	 * for setting the mutex type changed.  While the pthreads library
	 * maintains a backward compatibility mode, there is a newer
	 * invocation format.
	 */
#if defined ( REDHAT_52 )
#define PTHREAD_MUTEX_SETKIND pthread_mutexattr_setkind_np

#elif defined ( REDHAT_62 )
#define PTHREAD_MUTEX_SETKIND pthread_mutexattr_settype

	/* Try to guess it... */
#elif defined ( PTHREAD_CREATE_JOINABLE )
#define PTHREAD_MUTEX_SETKIND pthread_mutexattr_settype

	/* This may produce a warning message, but the         *
	 * pthreads library is, in fact, backwards compatible. */
#else
#define PTHREAD_MUTEX_SETKIND pthread_mutexattr_setkind_np

#endif



class Mutex : public threadIostreamBase
{
public:
  enum TYPE
  {
    FAST        = 0,  /* lock();lock() causes a deadlock  */
    RECURSIVE   = 1,  /* # unlocks() must equal # locks() */
    ERROR_CHECK = 2   /* second lock() fails with EDEADLK */
  };
  enum TRY_LOCK_STATUS_T
  {
    SUCCESS        = ::SUCCESS,
    FAILURE        = ::FAILURE,
    ALREADY_LOCKED = 3
  };

protected:
  Mutex::TYPE          mutexType;
  pthread_mutexattr_t  mutexAttributes;
  pthread_mutex_t      mutexData;

public:
  Mutex ( Mutex::TYPE  theMutexType = Mutex::RECURSIVE );

  virtual ~Mutex();

  status_t                 lock();
  Mutex::TRY_LOCK_STATUS_T trylock();
  status_t                 unlock();

	/* Note:  isLocked() will always report FALSE for RECURSIVE *
	 * mutexes when called in the same thread it was locked in. */
  BOOLEAN                  isLocked();


	/* Convenience methods */
  status_t                 lock    ( const char * theErrorLocation );
  status_t                 unlock  ( const char * theErrorLocation );
  Mutex::TRY_LOCK_STATUS_T trylock ( const char * theErrorLocation );



  pthread_mutex_t     & getMutexData()       { return mutexData;       }
  pthread_mutexattr_t & getMutexAttributes() { return mutexAttributes; }



  ostream & printObjectNonConst ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString );

	/* threadIostreamBase class interface *
	 * Note:  Casts away const'ness!      */
  virtual ostream & printObject ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString ) const;


private:  /* These two should *NEVER* be invoked.   Lets make sure of that! */
  Mutex ( const Mutex & theMutex );
  Mutex & operator= ( const Mutex & theMutex );
};


#endif /* MUTEX_H */

