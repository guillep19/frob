/*
 * Common Header file stuff.  (Macros, #defines, base classes, etc.)
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


#ifndef THREAD_COMMON_H
#define THREAD_COMMON_H

/*
 * Common includes.
 */

#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#include <errno.h>


/*
 * Under gcc 3.2.3, to discourage the use of the now obsolete iostream.h
 * interface, we are treated to warning messages during compilation.
 * However, rewriting the code will break compatibility with older systems,
 * and the iostream.h file still provides the precise interface required.
 * So, for now, we are just disabling these messages.
 */
#include <iostream>
#include <iomanip>
using namespace std;


	/* Helps to keep data synchronized in the tests... */
#ifndef FLUSHALL
#define FLUSHALL FLUSHALL
inline void FLUSHALL()
{
  fflush ( stdout );
  fflush ( stderr );
  cout << flush;
  cerr << flush;
}
#endif /* ! FLUSHALL */

	/* With RedHat 7.1/7.2, g++ 3.0 and up, we have an updated *
	 * iostreams library that breaks backwards compatibility.  */
#if  (                                                                        \
             ( defined ( THREADS_FORCE_NEW_IOSTREAMS_INTERFACE_STANDARD ) )   \
          || ( defined ( __GNUC__ ) && ( __GNUC__ >= 3 )                  ) ) \
  && ( /*NOT*/ !                                                              \
             ( defined ( THREADS_FORCE_OLD_IOSTREAMS_INTERFACE_STANDARD ) ) )
#define                 _THREADS_NEW_IOSTREAMS_INTERFACE_STANDARD
#else
#define                 _THREADS_OLD_IOSTREAMS_INTERFACE_STANDARD
#endif

	/* Print "(null)" for "ostream << (char*)NULL", like the old iostreams
	 * library did, as opposed to crashing, as the new iostreams library
	 * does.
	 */
#ifdef _THREADS_NEW_IOSTREAMS_INTERFACE_STANDARD
#ifndef _FIX_IOSTREAMS_NULL_OUTPUT_VALUES
#define _FIX_IOSTREAMS_NULL_OUTPUT_VALUES
inline std::ostream & operator << ( std::ostream & theOstream,
				    const char   * theString  )
{
  if ( theString == (const char *) NULL )
    return std::operator<< ( theOstream, "(null)" );
  else
    return std::operator<< ( theOstream, theString );
}

        /* And "(nil)" for (void*)NULL */
inline std::ostream & operator << ( std::ostream & theOstream,
				    void         * thePointer )
{
  if ( thePointer == NULL )
    return std::operator<< ( theOstream, "(nil)" );
  else
    return theOstream . operator<< ( thePointer );
}
#endif /* ! _FIX_IOSTREAMS_NULL_OUTPUT_VALUES */
#endif /* _THREADS_NEW_IOSTREAMS_INTERFACE_STANDARD */




/*
 * Convenient macros.
 */

#ifndef NULL_CHAR
#define NULL_CHAR '\0'
#endif

	    /* Print out the current error. */
#define PERROR_OSTREAM(PREFIX_STRING,SUFFIX_STRING,OSTREAM)               \
          (OSTREAM) << (PREFIX_STRING) << "[errno=" << errno << "  \""    \
                    << strerror ( errno ) << "\"]" << (SUFFIX_STRING)

#define PERROR(PREFIX_STRING)  PERROR_OSTREAM( (PREFIX_STRING), "\n", cerr)


/*
 * Standard (machine-independent) types...
 */
#ifndef HAS_STANDARD_TYPES
#define HAS_STANDARD_TYPES

typedef char                 int1;
typedef signed short int     int2;
typedef signed long  int     int4;
typedef signed long  long    int8;

typedef unsigned char        u_int1;
typedef unsigned short int   u_int2;
typedef unsigned long  int   u_int4;
typedef unsigned long  long  u_int8;

#endif /* HAS_STANDARD_TYPES */


/*
 * Boolean type.
 */
#ifndef HAS_BOOLEAN
#define HAS_BOOLEAN

#ifdef NO_TCM_COMPATIBILITY

/* My preferred version won't work.
 * Creates incompatibilities with TCM.
 */
enum BOOLEAN
{
  FALSE = 0,
  TRUE  = (!FALSE)
};

inline ostream &
operator << ( ostream & theOstream, const BOOLEAN & theBoolean )
{
  theOstream << ( ( theBoolean == TRUE ) ? "TRUE" : "FALSE" );
  return theOstream;
}

#else /* NO_TCM_COMPATIBILITY is NOT defined */


/* TCM requires this form for BOOLEAN for compatibility. */

#ifdef __SUNPRO_CC
typedef int bool;
#endif

typedef bool BOOLEAN;

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#endif /* NO_TCM_COMPATIBILITY */

#endif /* HAS_BOOLEAN */

#define TO_BOOLEAN(X)  ((X) ? TRUE : FALSE)



/*
 * Status type.
 */
#ifndef HAS_STATUS
#define HAS_STATUS

enum status_t
{
  FAILURE = FALSE,
  SUCCESS = TRUE
};

#endif /* ! HAS_STATUS */

#define TO_STATUS(X)  ((X) ? SUCCESS : FAILURE)

#ifndef HAS_STATUS_IOSTREAM
#define HAS_STATUS_IOSTREAM
inline ostream &
operator << ( ostream & theOstream, const status_t & theStatus )
{
  theOstream << ( ( theStatus == SUCCESS ) ? "SUCCESS" : "FAILURE" );
  return theOstream;
}
#endif  /* ! HAS_STATUS_IOSTREAM */




/*
 * Abstract iostream base class.
 * (Allows subclasses to print themselves out, nicely...)
 */
class threadIostreamBase
{
public:
	/* Force subclasses to use virtual destructors. */
  virtual ~threadIostreamBase() {}

	/* Syntactic sugar. */
  inline  ostream & operator << ( ostream & theOstream ) const
				  { return printObject ( theOstream, 0, "" ); }

	/* Note:  Pure-virtual method.  Must be defined in subclasses. */
  virtual ostream & printObject ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString ) const = 0;

};

class threadIostreamIndent : public threadIostreamBase
{
protected:
  u_int4       indent;
  const char * suffixString;

public:
  threadIostreamIndent ( u_int4       theIndent       = 0,
			 const char * theSuffixString = (const char *)NULL )
    : indent       ( theIndent       ),
      suffixString ( theSuffixString )
    {}

  virtual ~threadIostreamIndent() { indent = 0; }
  
  u_int4  getIndent() const             { return indent;      }
  void    setIndent( u_int4 theIndent ) { indent = theIndent; }

  const char * getSuffixString() const  { return suffixString; }
  void         setSuffixString(  const char * theSuffixString )
					{ suffixString = theSuffixString; }
  BOOLEAN hasPositiveLengthSuffixString() const
    { return TO_BOOLEAN(   (   getSuffixString()  != (const char *)NULL  )
			&& ( *(getSuffixString()) != NULL_CHAR           ) ); }


  virtual ostream & printObject ( ostream &    theOstream,
				  u_int4       theIndent,
				  const char * thePrefixString ) const
  {
    u_int4 i = getIndent() + theIndent;

    theOstream << thePrefixString;

    for ( ;  i>100;  i -= 100 )
      theOstream << "                                                                                                    ";
    for ( ;  i>10;  i -= 10 )
      theOstream << "          ";

    for ( ;  i>0;  i-- )
      theOstream << " ";

    if ( hasPositiveLengthSuffixString() == TRUE )
      theOstream << getSuffixString();

    return theOstream;
  }
};

inline ostream &
operator << ( ostream & theOstream, const threadIostreamBase & theIostreamBase)
{
  return theIostreamBase . printObject ( theOstream, 0, "" );
}




inline ostream &
threadTimestamp( ostream & theOstream = cerr )
{
  struct timeval currentTime;

  if ( gettimeofday ( & currentTime, (struct timezone *)NULL ) == -1 )
    theOstream << "[??.????]  ";

  else
  {
    theOstream << "[" << setw(2) << (currentTime.tv_sec  % 60)
	       << "." << setfill('0')
	              << setw(4) << ((currentTime.tv_usec + 50) / 100)
	              << setfill(' ')
	       << "]  ";
  }

  return theOstream;
}



#endif /* THREAD_COMMON_H */

