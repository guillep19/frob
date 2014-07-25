
/*****************************************************************************
 * 
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 2001 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmHandle.cc
 *
 * ABSTRACT: Basic definitions needed by TCM,
 *           Created to avoid inlining -DTHREADED elements.
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmHandle.cc,v $ 
 * $Revision: 1.2 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmHandle.cc,v $
 * Revision 1.2  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.1  2001/10/23 22:52:59  da0g
 * Added Threading support.  Cached lastchild.
 *
 *
 *****************************************************************************/

#include "tcmHandle.h"

#ifdef THREADED
#include "Mutex.H"
#endif /* THREADED */


Ref_Count::Ref_Count()
  : refCount ( 0 )
{
#ifdef THREADED
  mutexVoidPtr = (void *) (new Mutex ( Mutex::ERROR_CHECK ));
#else  /* THREADED */
  mutexVoidPtr = (void *) NULL;
#endif /* THREADED */
}


/*virtual*/
Ref_Count::~Ref_Count()
{
#ifdef THREADED
  if ( mutexVoidPtr != (void *) NULL )
  {
    delete ((Mutex *) mutexVoidPtr);
    mutexVoidPtr = (void *) NULL;
  }
  else
  {
    fprintf ( stderr, "[Ref_Count:~Ref_Count]  Error:  mutexVoidPtr was NULL."
	      "  Perhaps this instance is being deleted multiple times?" );
  }
#endif /* THREADED */
}


BOOLEAN
Ref_Count::lockRefCountMutex ( const char * theErrorLocation )
{
#ifdef THREADED
  if ( mutexVoidPtr != (void *) NULL )
    return TO_BOOLEAN ( ((Mutex *) mutexVoidPtr) -> lock ( theErrorLocation )
			== SUCCESS );
  else
  {
    fprintf ( stderr, "[Ref_Count::lockRefCountMutex]  Error:  mutexVoidPtr "
	      "was NULL.  Perhaps this instance was already deleted?" );
    return FALSE;
  }
#else /* THREADED */
  (void)(&theErrorLocation);
  return TRUE;
#endif /* THREADED */
}
      

BOOLEAN
Ref_Count::unlockRefCountMutex ( const char * theErrorLocation )
{
#ifdef THREADED
  if ( mutexVoidPtr != (void *) NULL )
    return TO_BOOLEAN ( ((Mutex *) mutexVoidPtr) -> unlock ( theErrorLocation )
			== SUCCESS );
  else
  {
    fprintf ( stderr, "[Ref_Count::unlockRefCountMutex]  Error:  mutexVoidPtr "
	      "was NULL.  Perhaps this instance was already deleted?" );
    return FALSE;
  }
#else /* THREADED */
  (void)(&theErrorLocation);
  return TRUE;
#endif /* THREADED */
}


void
Ref_Count::increment(void)
{
  lockRefCountMutex("increment");
  refCount++;
  unlockRefCountMutex("increment");
}


void
Ref_Count::decrement(void)
{
  lockRefCountMutex("decrement");

  refCount--;
  if ( refCount <= 0 )
  {
	/* Unlock must occur after refCount value is tested. *
	 * Otherwise we have a threading race condition.     */
    unlockRefCountMutex("decrement");
    delete this;
  }
  else
  {
    unlockRefCountMutex("decrement");
  }
}

