/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#ifndef stdtypes_H
#define stdtypes_H

using namespace std;

typedef char                  int1;
typedef signed short int      int2;
typedef signed long  int      int4;
typedef signed long  long     int8;

typedef unsigned char         u_int1;
typedef unsigned short int    u_int2;
typedef unsigned long  int    u_int4;
typedef unsigned long  long   u_int8;


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
#if  (           ( defined ( TDL_FORCE_NEW_IOSTREAMS_INTERFACE_STANDARD ) )   \
              || ( defined ( __GNUC__ ) && ( __GNUC__ >= 3 )              ) ) \
  && ( /*NOT*/ ! ( defined ( TDL_FORCE_OLD_IOSTREAMS_INTERFACE_STANDARD ) ) )
#define                 _TDL_NEW_IOSTREAMS_INTERFACE_STANDARD
#else
#define                 _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD
#endif

	/* For convenience */
#ifndef DISABLE_IOSTREAM
#ifdef    _TDL_NEW_IOSTREAMS_INTERFACE_STANDARD
#define   _TDL_NEW_IOSTREAMS_INTERFACE_STANDARD_ENABLE_IOSTREAM
#endif /* _TDL_NEW_IOSTREAMS_INTERFACE_STANDARD */

#ifdef    _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD
#define   _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD_ENABLE_IOSTREAM
#endif /* _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD */
#endif /* ! DISABLE_IOSTREAM */

	/* Print "(null)" for "ostream << (char*)NULL", like the old iostreams
	 * library did, as opposed to crashing, as the new iostreams library
	 * does.
	 */
#ifdef _TDL_NEW_IOSTREAMS_INTERFACE_STANDARD_ENABLE_IOSTREAM
#ifndef _FIX_IOSTREAMS_NULL_OUTPUT_VALUES
#define _FIX_IOSTREAMS_NULL_OUTPUT_VALUES
inline std::ostream & operator << ( std::ostream & theOstream,
				    const char * theString )
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
#endif /* ! _FIXED_IOSTREAMS_NULL_OUTPUT_VALUES */
#endif /* _TDL_NEW_IOSTREAMS_INTERFACE_STANDARD */



#ifndef NULL_CHAR
#define NULL_CHAR '\0'
#endif

/* For the non-macintosh platforms, classes with abstract methods          *
 * declare them as pure-virtual.  E.g. "void foo() = 0;" vs "void foo();"  *
 * (This is normally declared in tcmBasics.h.)                             */
#ifndef TCM_PURE_VIRTUAL_METHOD
#ifndef macintosh
#define TCM_PURE_VIRTUAL_METHOD = 0
#else
#define TCM_PURE_VIRTUAL_METHOD
#endif /* #ifndef macintosh */
#endif /* #ifndef TCM_PURE_VIRTUAL_METHOD */


#ifdef NEEDS_BOOL
enum BOOLEAN
{
  FALSE = 0,
  TRUE  = (!FALSE)
};

#ifndef DISABLE_IOSTREAM
inline ostream &
operator << ( ostream & theOstream, const BOOLEAN & theBoolean )
{
  theOstream << ( ( theBoolean == TRUE ) ? "TRUE" : "FALSE" );
  return theOstream;
}
#endif /* ! DISABLE_IOSTREAM */

#endif  /* NEEDS_BOOL */


#ifndef TO_BOOLEAN
#define TO_BOOLEAN(X)  ((X) ? TRUE : FALSE)
#endif /* ! TO_BOOLEAN */



#ifndef HAS_STATUS
#define HAS_STATUS
enum status_t
{
  FAILURE = FALSE,
  SUCCESS = TRUE
};
#endif  /* ! HAS_STATUS */

#ifndef HAS_STATUS_IOSTREAM
#ifndef DISABLE_IOSTREAM
#define HAS_STATUS_IOSTREAM
inline ostream &
operator << ( ostream & theOstream, const status_t theStatus )
{
  theOstream << ( ( theStatus == SUCCESS ) ? "SUCCESS" : "FAILURE" );
  return theOstream;
}
#endif /* ! DISABLE_IOSTREAM */
#endif  /* ! HAS_STATUS_IOSTREAM */



#ifndef DISABLE_IOSTREAMBASE
class  iostreamBase
{
public:
  enum CONSTANTS
  {
    MAXIMUM_INDENT = 127
  };

  virtual ~iostreamBase () {}

  inline static void createSubIndentString (
		  char   theSubIndentString [ iostreamBase::MAXIMUM_INDENT ],
	    const char * theCurrentIndentString = ""                         );

  inline ostream & operator << ( ostream & theOstream ) const
				     { return printObject ( theOstream, "" ); }

  virtual ostream & printObject (
	     ostream    & theOstream,
	     const char * theIndentString = "") const TCM_PURE_VIRTUAL_METHOD ;
};

inline /*static*/ void
iostreamBase::createSubIndentString (
		  char   theSubIndentString [ iostreamBase::MAXIMUM_INDENT ],
	    const char * theCurrentIndentString /* = "" */                   )
{
  strncpy ( theSubIndentString,
	    theCurrentIndentString,
	    iostreamBase::MAXIMUM_INDENT - 1 );
  theSubIndentString [ iostreamBase::MAXIMUM_INDENT - 1 ] = NULL_CHAR;

  if ( strlen ( theSubIndentString ) <= (iostreamBase::MAXIMUM_INDENT - 3) )
    strcat ( theSubIndentString, "  " );

  else if (strlen ( theSubIndentString ) == (iostreamBase::MAXIMUM_INDENT - 2))
    strcat ( theSubIndentString, " " );
}

inline ostream &
operator << ( ostream & theOstream, const iostreamBase & theIostreamBase )
{
  return theIostreamBase . printObject ( theOstream, "" );
}
#endif /* ! DISABLE_IOSTREAMBASE */



  /*
   * A nice, safe, double-checked strings equal routine.
   */
inline BOOLEAN StringEqual ( const char * theFirstString,
			     const char * theSecondString )
{
  if ( theFirstString == (const char *)NULL )
  {
    cerr << "[StringEqual]  Error:  First String is NULL." << endl;
    return FALSE;
  }

  else if ( theSecondString == (const char *)NULL )
  {
    cerr << "[StringEqual]  Error:  Second String is NULL." << endl;
    return FALSE;
  }
  
  else 
  {
    return strcmp ( theFirstString, theSecondString ) == 0 ? TRUE : FALSE;
  }
}


  /*
   * String duplication is sometimes necessary...
   * Note:  This returns heap-memory that must be delete'ed!!!!
   */
inline char * NewString ( const char * theStringToDuplicate )
{
  if ( theStringToDuplicate == (const char *)NULL )
  {
    cerr << "[NewString]  Error:  Original String is NULL!" << endl;
    return (char *)NULL;
  }
  else
  {
    size_t stringLength = strlen ( theStringToDuplicate ) + 1;
    char * newString    = new char [ stringLength ];

    strncpy ( newString, theStringToDuplicate, stringLength );

	/* Better play it safe and insure null-termination. */
    newString [ stringLength - 1 ] = NULL_CHAR;

    return newString;
  }
}



#define MALLOC(X) ((X *)(MallocWithErrorchecking(sizeof(X))))
inline void * MallocWithErrorchecking( size_t theSize )
{
  void * value = malloc ( theSize );
  if ( value == (void *)NULL )
  {
    cerr << "[mallocWithErrorchecking]  Error:  "
	 << "Malloc Failed to allocate a space of size " << theSize
	 << ".\nAborting...\n" << endl;
    exit(-1);  /* There's no recovery option here... */
  }
  memset ( value, 0, theSize );
  return value;
}



  /* The nice thing about standards is that there are so many to choose from.*/
#if    ( defined(sun) || defined(__sun) || defined (__sun__) ) \
    && !defined(__SUNPRO_CC)

inline time_t
_TDL_MAKE_TIME_LOCAL ( struct tm * theTM ) { return timelocal ( theTM ); }
inline time_t
_TDL_MAKE_TIME_GMT   ( struct tm * theTM ) { return timegm    ( theTM ); }

#else /* defined ( sun || __sun || __sun__ ) && !defined(__SUNPRO_CC) */

inline time_t
_TDL_MAKE_TIME_LOCAL ( struct tm * theTM ) { return mktime    ( theTM ); }
inline time_t
_TDL_MAKE_TIME_GMT   ( struct tm * theTM ) { return mktime    ( theTM ); }

#endif /* defined ( sun || __sun || __sun__ ) */



	/* Useful routine to have around. */
inline struct timeval getTimeOfDay()
{
  struct timeval currentTime;
  if ( gettimeofday ( & currentTime, (struct timezone *) NULL ) != 0 )
  {
    cerr << "[getTimeOfDay]  Error:  gettimeofday() failed with errno="
	 << errno << endl;
    currentTime . tv_sec  = 0;
    currentTime . tv_usec = 0;
  }
  return currentTime;
}


/*****************************************************************************
 * da0g:  START:  "Borrowing" this from a personal side project of mine.
 * Consider it BSD-licensed.  I don't feel like reimplementing it.
 *****************************************************************************/
inline void
normalizeTime ( struct timeval & theTimeVal )  /* CHANGED Reference! */
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

inline struct timeval
operator- ( struct timeval theTimeVal, /* Copy onto stack. */
	    int8           theMicrosecondsToSubtract )
{
  theTimeVal . tv_sec  -= theMicrosecondsToSubtract / 1000000;
  theTimeVal . tv_usec -= theMicrosecondsToSubtract % 1000000;
  normalizeTime ( theTimeVal );
  return theTimeVal;
}

inline struct timeval
operator+ ( struct timeval theTimeVal, /* Copy onto stack. */
	    int8           theMicrosecondsToAdd )
{
  theTimeVal . tv_sec  += theMicrosecondsToAdd / 1000000;
  theTimeVal . tv_usec += theMicrosecondsToAdd % 1000000;
  normalizeTime ( theTimeVal );
  return theTimeVal;
}

inline struct timeval
operator-= ( struct timeval & theTimeVal, /* CHANGED Reference! */
	     int8             theMicrosecondsToSubtract )
{
  theTimeVal . tv_sec  -= theMicrosecondsToSubtract / 1000000;
  theTimeVal . tv_usec -= theMicrosecondsToSubtract % 1000000;
  normalizeTime ( theTimeVal );
  return theTimeVal;
}

inline struct timeval
operator+= ( struct timeval & theTimeVal, /* CHANGED Reference! */
	     int8             theMicrosecondsToAdd )
{
  theTimeVal . tv_sec  += theMicrosecondsToAdd / 1000000;
  theTimeVal . tv_usec += theMicrosecondsToAdd % 1000000;
  normalizeTime ( theTimeVal );
  return theTimeVal;
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
operator+= (       struct timeval & theFirstTimeVal,   /* CHANGED Reference! */
             const struct timeval & theSecondTimeVal ) /* Reference!         */
{
  theFirstTimeVal . tv_sec  += theSecondTimeVal . tv_sec;
  theFirstTimeVal . tv_usec += theSecondTimeVal . tv_usec;
  normalizeTime ( theFirstTimeVal );
  return theFirstTimeVal;
}

inline struct timeval
operator-= (       struct timeval & theFirstTimeVal,   /* CHANGED Reference! */
             const struct timeval & theSecondTimeVal ) /* Reference!         */
{
  theFirstTimeVal . tv_sec  -= theSecondTimeVal . tv_sec;
  theFirstTimeVal . tv_usec -= theSecondTimeVal . tv_usec;
  normalizeTime ( theFirstTimeVal );
  return theFirstTimeVal;
}

inline int
operator > ( struct timeval theFirstTimeVal,   /* Copy onto stack. */
	     struct timeval theSecondTimeVal ) /* Copy onto stack. */
{
  normalizeTime ( theFirstTimeVal  );
  normalizeTime ( theSecondTimeVal );
  return (   (     theFirstTimeVal.tv_sec  >  theSecondTimeVal.tv_sec    )
	  || (   ( theFirstTimeVal.tv_sec  == theSecondTimeVal.tv_sec  )
	      && ( theFirstTimeVal.tv_usec >  theSecondTimeVal.tv_usec ) ) );
}

inline int
operator < ( struct timeval theFirstTimeVal,   /* Copy onto stack. */
	     struct timeval theSecondTimeVal ) /* Copy onto stack. */
{
  normalizeTime ( theFirstTimeVal  );
  normalizeTime ( theSecondTimeVal );
  return (   (     theFirstTimeVal.tv_sec  <  theSecondTimeVal.tv_sec    )
	  || (   ( theFirstTimeVal.tv_sec  == theSecondTimeVal.tv_sec  )
	      && ( theFirstTimeVal.tv_usec <  theSecondTimeVal.tv_usec ) ) );
}

inline int
operator >= ( struct timeval theFirstTimeVal,   /* Copy onto stack. */
	      struct timeval theSecondTimeVal ) /* Copy onto stack. */
{
  normalizeTime ( theFirstTimeVal  );
  normalizeTime ( theSecondTimeVal );
  return (   (     theFirstTimeVal.tv_sec  >= theSecondTimeVal.tv_sec    )
	  || (   ( theFirstTimeVal.tv_sec  == theSecondTimeVal.tv_sec  )
	      && ( theFirstTimeVal.tv_usec >= theSecondTimeVal.tv_usec ) ) );
}

inline int
operator <= ( struct timeval theFirstTimeVal,   /* Copy onto stack. */
	      struct timeval theSecondTimeVal ) /* Copy onto stack. */
{
  normalizeTime ( theFirstTimeVal  );
  normalizeTime ( theSecondTimeVal );
  return (   (     theFirstTimeVal.tv_sec  <= theSecondTimeVal.tv_sec    )
	  || (   ( theFirstTimeVal.tv_sec  == theSecondTimeVal.tv_sec  )
	      && ( theFirstTimeVal.tv_usec <= theSecondTimeVal.tv_usec ) ) );
}
 
inline int
operator == ( struct timeval theFirstTimeVal,   /* Copy onto stack. */
	      struct timeval theSecondTimeVal ) /* Copy onto stack. */
{
  normalizeTime ( theFirstTimeVal  );
  normalizeTime ( theSecondTimeVal );
  return (   ( theFirstTimeVal.tv_sec  == theSecondTimeVal.tv_sec  )
	  && ( theFirstTimeVal.tv_usec == theSecondTimeVal.tv_usec ) );
}
 
inline int
operator != ( struct timeval theFirstTimeVal,   /* Copy onto stack. */
	      struct timeval theSecondTimeVal ) /* Copy onto stack. */
{
  normalizeTime ( theFirstTimeVal  );
  normalizeTime ( theSecondTimeVal );
  return (   ( theFirstTimeVal.tv_sec  != theSecondTimeVal.tv_sec  )
	  || ( theFirstTimeVal.tv_usec != theSecondTimeVal.tv_usec ) );
}
 
inline int4
timeDifferenceInMilliseconds ( const struct timeval & theFirstTimeVal,
                               const struct timeval & theSecondTimeVal )
{
  return   ( (theFirstTimeVal.tv_sec  - theSecondTimeVal.tv_sec ) * 1000000 )
         + (  theFirstTimeVal.tv_usec - theSecondTimeVal.tv_usec            );
}

#ifndef DISABLE_IOSTREAM
inline ostream &
timestamp( ostream & theOstream = cerr )
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

/* "struct timeval" type.                       *
 * Caveat Emptor:  This is *NOT* thread-safe!!! */
inline ostream &
operator << ( ostream & theOstream, const struct timeval & theTimeval )
{
  char        oldFillChar = theOstream.fill();
  struct tm * currentTime = localtime ( & theTimeval.tv_sec );
  

  theOstream << setfill ( '0' )
             << setw(4) << (currentTime->tm_year + 1900) << "."
             << setw(2) << (currentTime->tm_mon  +    1) << "."
             << setw(2) << currentTime->tm_mday          << "-"
             << setw(2) << currentTime->tm_hour          << ":"
             << setw(2) << currentTime->tm_min           << ":"
             << setw(2) << currentTime->tm_sec           << "."
             << setw(6) << theTimeval.tv_usec
             << setfill ( oldFillChar );
  return theOstream;
}
#endif /* NOT DISABLE_IOSTREAM */

/*****************************************************************************
 * da0g:  END:  "Borrowed" code from my personal side project.
 *****************************************************************************/



	/* Useful routine to have around.  Returns ABS(first-second) */
inline struct timeval getTimeDifference ( struct timeval theFirstTime,
					  struct timeval theSecondTime )
{
  if ( theFirstTime >= theSecondTime )
    return theFirstTime - theSecondTime;
  else
    return theSecondTime - theFirstTime;
}


inline double timeToDouble ( struct timeval theTime )
{
  return double ( theTime.tv_sec ) + ( double ( theTime.tv_usec ) / 1E6 );
}

	/* Is theTime >= theReferenceTime.
	 * Allow == to deal with fencepost errors on systems that
	 * have a coarse granularity for time.
	 */
inline BOOLEAN getHasTimeBeenReached ( struct timeval theTime,
				       struct timeval theReferenceTime )
{
  return TO_BOOLEAN ( theTime >= theReferenceTime );
}



  /* This is a nifty macro to have around...
   *   -- Watch out for side effects though!!!
   *      Ie: FLOATING_POINT_EQUIVALENT (i++,j++,1E-6) yields i+=2, j+=2...
   */
#define FLOATING_POINT_EQUIVALENT(X,Y,EPSILON)  \
                          ( ( ((X)>(Y)) ? ((X)-(Y)) : ((Y)-(X)) ) < (EPSILON) )

	/* Standard macros.   Beware side effects!! */
#define ABS(X)              ( ((X) >= 0) ? (X) : (- (X)) )
#define MIN(X,Y)            ( ((X) <= (Y)) ? (X) : (Y) )
#define MAX(X,Y)            ( ((X) >= (Y)) ? (X) : (Y) )
#define CLAMP(X,MINX,MAXX)  ( MIN( MAX( (X), (MINX) ), (MAXX) ) )
#define SQUARE(X)           ( (X) * (X) )


	/* Standard conversion macros... */
#define  RADIANS_TO_DEGREES(RADIANS)   ( double(RADIANS) * double(180) / M_PI )
#define  DEGREES_TO_RADIANS(DEGREES)   ( double(DEGREES) * M_PI / double(180) )

	/* Time constants */
#define  MICROSECONDS_PER_SECOND 1000000
#define  MILLISECONDS_PER_SECOND 1000


#endif /* stdtypes_H */

