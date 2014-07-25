/*****************************************************************************
 *
 * PROJECT: Task Control Management.
 *
 * (c) Copyright 1996 Reid Simmons.  All rights reserved.
 *
 * FILE: tcmBasics.h
 *
 * ABSTRACT: Basic definitions needed by TCM
 *
 * $Source: /afs/cs.cmu.edu/project/TCA/Master/tcaV8/tcm/src/tcmBasics.h,v $ 
 * $Revision: 1.21 $
 * $Date: 2009/01/15 17:00:05 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: tcmBasics.h,v $
 * Revision 1.21  2009/01/15 17:00:05  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.20  2008/07/16 06:15:10  reids
 * Updates for newer (pickier) compilers
 *
 * Revision 1.19  2008/07/11 15:47:03  reids
 * Merged the two previous ways that exceptions were implemented (the original
 * version, and one for TDL).  Extended to handle distributed exceptions.
 * Added the TRACE_UBER flag for even more detailed tracing.
 * Added API functions: TCM_FailureNode and TCM_ExceptionHandlerNode.
 *
 * Revision 1.18  2003/04/17 21:12:29  da0g
 * Added MSECS_PRINTF_STRING.
 *
 * Revision 1.17  2003/01/29 20:32:50  da0g
 * Added CYGWIN/Windows tests to #ifdefs.
 *
 * Revision 1.16  2002/07/11 03:54:07  da0g
 * Added NULL_CHAR macro.
 * Added TCM_PURE_VIRTUAL_METHOD macro.
 * Fixed STRING typedef #ifdefs.
 * Addressed String = (char*) vs (const char *) issues.
 *
 * Revision 1.15  2002/06/26 16:50:24  reids
 * Removed signatures of library functions that are already defined.
 *
 * Revision 1.14  2002/03/22 02:27:47  da0g
 * Added Exception-Handler-Ordering code.
 *
 * Revision 1.13  2001/10/23 22:52:59  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.12  2000/07/05 23:16:25  da0g
 * Modified MSecs to be a 8-byte integer, thereby fixing the
 * time-wrapping bug that occurs every 7 weeks or so.
 *
 * Added includes/#ifdefs for compatibility with Solaris CC.
 *
 * Revision 1.11  2000/02/03 21:24:45  reids
 * Removed compiler warning for glibc 2.2
 *
 * Revision 1.10  1999/10/21 21:32:33  reids
 * Changed list.cc and list.h to tcmList.cc and tcmList.h to avoid conflicts
 *   with other occurrences of those names in other packages.
 *
 * Revision 1.9  1999/06/06 13:48:09  reids
 * Changes to TCM 1.3.0:
 *   New list data structures.
 *   New, generalized logging interface.
 *   Fixed firstChild to work if there are no children.
 *   Fixed code to deallocate a node.
 *   In certain circmumstances, don't send termination signal when task
 *     itself terminating.
 *
 * Revision 1.8  98/09/15  18:45:23  da0g
 * Enhanced exceptions to support multiple-name resolution and Ref_Count (automatically-destroyed) Data.
 * 
 * Revision 1.7  1998/06/02 10:39:43  reids
 * Increased overall efficiency of TCM.
 *
 * Revision 1.6  98/03/06  13:12:47  reids
 * Added a way to stop the compiler from complaining about unused function
 *   arguments (apparently, g++ ignores the "#pragma unused" statement).
 * 
 * Revision 1.5  98/03/06  12:42:36  reids
 * Modifications made to support Solaris.
 * 
 * Revision 1.4  98/01/30  14:51:06  reids
 * Updated to compile under gcc 2.7.2 and under Linux.
 * Also, made STRING "const char *" and changed API to take const arguments,
 *   where applicable.
 * 
 * Revision 1.3  97/12/29  17:06:34  reids
 * Version that has the basic functionality needed to support TDL.
 * 
 * Revision 1.2  97/12/04  17:50:24  reids
 * Another fairly stable version (except that monitors do not quite work)
 * 
 * Revision 1.1  97/11/21  14:06:42  reids
 * First release of TCM -- seems to be a stable version
 * 
 *
 *****************************************************************************/

#ifndef INCbasics
#define INCbasics

// The definition of GNUC_PREREQ in g++ 3.4.2 is buggy!
#undef __GNUC_PREREQ
#define __GNUC_PREREQ(maj,min) (((__GNUC__ << 16) + __GNUC_MINOR__) >= (((maj) << 16) + (min)))

// Functions that should be defined elsewhere, but are not
#include <sys/types.h>
#include <sys/time.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#if !defined(macintosh) && !defined(linux) && !defined(__svr4__) \
 && !defined(__CYGWIN__)
extern "C" void bzero(char *b, int length);
#else
#define bzero(a, b) memset(a, 0, b)
#endif
//extern "C" int select (int, fd_set*, fd_set*, fd_set*, struct timeval*);
#if !(__GLIBC__ >= 2 && __GLIBC_MINOR__ > 0)
extern "C" int ftime(struct timeb *tp);
#endif
//#if !defined(__sgi) && !defined(__svr4__) && !defined(__SUNPRO_CC)
//extern "C" int gettimeofday(struct timeval *, struct timezone *);
//#endif

// So that the compiler will stop complaining about unused arguments
#define UNUSED(arg) (void)(&arg)

#ifdef macintosh
#include <stdtypes.h>
#elif !defined(linux) && !defined(__svr4__) && !defined(__sgi) \
   && !defined(__SUNPRO_CC) && !defined(__CYGWIN__)
#include <sys/stdtypes.h>
#endif

#ifndef NULL
#define NULL (0)
#endif

#ifndef NULL_CHAR
#define NULL_CHAR '\0'
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

/* For the non-macintosh platforms, classes with abstract methods          *
 * declare them as pure-virtual.  E.g. "void foo() = 0;" vs "void foo();"  */
#ifndef macintosh
#define TCM_PURE_VIRTUAL_METHOD = 0
#else
#define TCM_PURE_VIRTUAL_METHOD
#endif


inline void BCOPY(void *from, void *to, size_t len) { memcpy(to, from, len); }

#ifdef __SUNPRO_CC
typedef int bool;
#endif

#ifndef _WINSOCK_  /* windows already has a BOOLEAN type */
#ifndef BOOLEAN
typedef bool BOOLEAN;
#endif
#endif /* _WINSOCK_ */

#include <string.h>


    /* For certain (really old) compilers, we wanted STRING to be char*,  *
     * not const-char*.  So, this ugliness should resolve the problem in  *
     * a more forward-compatible manner.                                  */
#if       defined ( TCM_FORCE_STRING_CHAR )
  typedef char * STRING;
#elif     defined ( TCM_FORCE_STRING_CONST_CHAR )
  typedef const char * STRING;
#elif !__GNUC_PREREQ(2,6)
  typedef char * STRING;  
#else
  typedef const char * STRING;  
#endif


const unsigned int MAXINT = 0xFFFFFFFF;

#ifdef macintosh
char *strdup(const char *str);
#endif

inline BOOLEAN streq(STRING str1, STRING str2) { return !(strcmp(str1, str2));}

/* "unsigned long" MSecs overflows & wraps back to zero    *
 *  every 7 weeks, 17 hours, 2 minutes and 47.295 seconds. */
#ifdef INT64_UNAVAILABLE
typedef unsigned long MSecs;
const MSecs INFINITE_TIME = 0xFFFFFFFF;
#define MSECS_PRINTF_STRING  "%ld"
#else
typedef unsigned long long MSecs;
const MSecs INFINITE_TIME = 0xFFFFFFFFFFFFFFFFULL;
#define MSECS_PRINTF_STRING  "%lld"
#endif

// This is ugly, but needed to get around an apparent egcs compiler bug
// Forces "secs" to be put on the stack (suggested by David Apfelbaum).
inline MSecs seconds(double secs) { return (MSecs)(1000.0 * (*&secs)); }

extern MSecs timeInMsecs (void);

inline MSecs absoluteTime(MSecs relTime) 
{ return (relTime == INFINITE_TIME ? INFINITE_TIME : relTime + timeInMsecs());}

#define DEFAULT_EXCEPTION_HANDLER_PRIORITY -1000

struct eqstr {
  bool operator() (STRING s1, STRING s2) const { return strcmp(s1, s2) == 0; }
};

#endif /* INCbasics */
