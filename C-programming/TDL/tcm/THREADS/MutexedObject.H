
/*
 * MutexedObject.H -- Base class for objects that utilize a mutex internally.
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


#ifndef MUTEXED_OBJECT_H
#define MUTEXED_OBJECT_H

#include "Mutex.H"

class MutexedObject
{
protected:
  Mutex  ourMutex;

public:
  MutexedObject( Mutex::TYPE theMutexType = Mutex::RECURSIVE )
    : ourMutex ( theMutexType )
  {}

  virtual ~MutexedObject();

public:
  Mutex & getMutex() { return ourMutex; }

protected:
  status_t                 lock    ( const char * theErrorLocation )
     { return getMutex() . lock    (              theErrorLocation ); }
  status_t                 unlock  ( const char * theErrorLocation )
     { return getMutex() . unlock  (              theErrorLocation ); }
  Mutex::TRY_LOCK_STATUS_T trylock ( const char * theErrorLocation )
     { return getMutex() . trylock (              theErrorLocation ); }
  

private:  /* These two should *NEVER* be invoked.   Lets make sure of that! */
  MutexedObject ( const MutexedObject & theMutexedObject );
  MutexedObject & operator= ( const MutexedObject & theMutexedObject );
};

#endif /* MUTEXED_OBJECT_H */

