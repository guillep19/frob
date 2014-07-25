/*****************************************************************************
 *
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmHandle.h
 *
 * ABSTRACT: Basic definitions needed by TCM
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmHandle.h,v $ 
 * $Revision: 1.11 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmHandle.h,v $
 * Revision 1.11  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.10  2009/01/06 18:08:51  reids
 * Fixed compiler error for machines with long pointer types
 *
 * Revision 1.9  2001/10/23 22:52:59  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.8  2000/07/05 23:19:49  da0g
 * Added a few extra comparison operators to reduce overhead
 * and simplify things.
 *
 * Revision 1.7  2000/02/03 21:24:46  reids
 * Removed compiler warning for glibc 2.2
 *
 * Revision 1.6  2000/02/02 22:09:47  da0g
 * Fixed improper templating bugs with tcmHandle's refIncr and refDecr
 *
 * Revision 1.5  2000/02/02 18:11:23  da0g
 * Corrected templating code.
 *
 * Revision 1.4  1999/02/14 01:11:50  da0g
 * Updated to work with g++ 2.8.1 and egcs.
 *
 * Revision 1.3  97/12/29  17:06:35  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.2  97/12/04  17:50:27  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:45  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 *****************************************************************************/

#ifndef INCtcmHandle
#define INCtcmHandle

#include "tcmBasics.h"

class Ref_Count
{
private:
  int    refCount;
  void * mutexVoidPtr;

 public:
  Ref_Count();

  virtual ~Ref_Count();

  BOOLEAN lockRefCountMutex   ( const char * theErrorLocation );
  BOOLEAN unlockRefCountMutex ( const char * theErrorLocation );
  void increment(void);
  void decrement(void);

private:
    /* These should never be invoked.  Lets make certain of that.  These   *
     * methods do NOT exist.  They will generate link-time errors if used. */
  Ref_Count( const Ref_Count & theRefCount );
  Ref_Count & operator=( const Ref_Count & theRefCount );
};


#define DECLARE_REF_COUNT_FUNCTIONS(type)   \
class type; extern void refIncr(type *ptr); \
class type; extern void refDecr(type *ptr);

#define INSTANTIATE_REF_COUNT_FUNCTIONS(type) \
void refIncr(type *ptr) { ptr->increment(); } \
void refDecr(type *ptr) { ptr->decrement(); } 


template <class T>
class tcmHandle
{
 public:
  tcmHandle() { refPtr = NULL; }
  tcmHandle(T *ptr) { _bind(ptr); }
  tcmHandle(tcmHandle<T> const &handle) { _bind(handle.refPtr); }
  ~tcmHandle() { _release(); }

  tcmHandle<T> &operator= (tcmHandle<T> const &handle) 
    { _release(); _bind(handle.refPtr); return *this; }
  tcmHandle<T> &operator= (T *ptr) { _release(); _bind(ptr); return *this; }

  bool operator== (tcmHandle<T> const &handle) const
    { return refPtr == handle.refPtr; }
  bool operator== (const T *ptr) const
    { return refPtr == ptr; }

  bool operator!= (tcmHandle<T> const &handle) const
    { return refPtr != handle.refPtr; }
  bool operator!= (const T *ptr) const
    { return refPtr != ptr; }

  // Silly, but some classes need it defined
  bool operator< (tcmHandle<T> const &handle) const 
    { return (long)refPtr < (long)handle.refPtr; }

  T * operator* () const { return refPtr; }
  T *operator-> () const { return refPtr; }

  int  isNotNull() const { return refPtr != (T *)NULL; }
  int  isNull()    const { return refPtr == (T *)NULL; }
  tcmHandle<T> & clear() { return operator= ( (T *)NULL ); }

 private:
  T * refPtr;

  void _release (void) { if (refPtr) { refDecr(refPtr); refPtr = NULL; } }
  void _bind (T *ptr) { refPtr = ptr; if (ptr) { refIncr(ptr); } }
};

template <class T>
inline tcmHandle<T> makeHandle(T *ptr)
{
  tcmHandle<T> handle(ptr);
  return handle;
}

#endif /* INChandle */
