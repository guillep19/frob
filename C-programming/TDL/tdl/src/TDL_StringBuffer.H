/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef TDL_STRING_BUFFER
#define TDL_STRING_BUFFER

  /* We need some standard defintions, which used to come from stdtypes.H
   * However, these have been supplanted by other definitions in tcm.h...
   * Note: _TDL_INTERNAL_ may be have been set elsewhere, in which case
   *       we need to keep it set.
   */
#ifndef _TDL_INTERNAL_
#define _TDL_INTERNAL_
#define _TDL_UNDEF_INTERNAL_STRING_BUFFER
#endif

#include "tdl.H"

#ifdef _TDL_UNDEF_INTERNAL_STRING_BUFFER
#undef _TDL_UNDEF_INTERNAL_ACTION_OR_VOID
#undef _TDL_INTERNAL_
#endif


class TDL_StringBuffer
{
protected:
  STRINGSTREAM  streamBuffer;
#if __GNUC_PREREQ(3,3)
  // Needed for ostringstream, because the str() function creates a temporary string that is GC'd once the cast 
  // operator completes, trashing the char * that results
  string tempStr;
#endif

public:
  TDL_StringBuffer() {}
  virtual ~TDL_StringBuffer();

  STRINGSTREAM & getInternalOstrstreamObject() { return streamBuffer; }

  TDL_StringBuffer & operator << ( int4         theInt    )
	{ getInternalOstrstreamObject() << theInt;    return *this; }
  TDL_StringBuffer & operator << ( const char * theString )
	{ getInternalOstrstreamObject() << theString; return *this; }

  operator STRING ()
#if __GNUC_PREREQ(3,3)
  {
    // It must be done this way for some unknown reason -- this is both from 
    // Nik Melchior and from observation. If we just set tempStr to be the
    // str() output, the string that is returned gets trashed.
    // Likewise, just returning str().c_str() does not work reliably.
    // Apparently, we need to create a new string, with its own copy of the
    // characters, and pass that around.  Sigh...
    string tempStr = getInternalOstrstreamObject() . str() . c_str();
    return tempStr.c_str();
  }
#else
	/* Implies "freeze" -- string will not change again until unfreeze() */
	{ return STRING(getInternalOstrstreamObject() . str()); }
#endif

#if !__GNUC_PREREQ(3,3)
	/* Allow the internal string to be changed / reallocated / destroyed */
  void unfreeze()
	{ getInternalOstrstreamObject() . freeze ( 0 ); }
#endif
};

/*
 * Some compilers may need these functions external to the class.
 * Red Hat 6.2 (egcs-2.91.66) does not..
 */
#ifdef TDL_STRING_BUFFER_NEEDS_OPERATOR_FUNCTIONS
inline TDL_StringBuffer &
operator << ( TDL_StringBuffer & theTdlStringBuffer, int4 theInt )
{
  return theTdlStringBuffer . operator << ( theInt );
}

inline TDL_StringBuffer &
operator << ( TDL_StringBuffer & theTdlStringBuffer, const char * theString )
{
  return theTdlStringBuffer . operator << ( theString );
}
#endif /* TDL_STRING_BUFFER_NEEDS_OPERATOR_FUNCTIONS */

#endif /* TDL_STRING_BUFFER */

