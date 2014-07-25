/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tdl/LICENSE.TXT)
 */

#define _TDL_INTERNAL_
#include <tdl.H>
#include "_TDL_AllocationFunction.H"
#include "_TDL_DistributedRegistryEntry.H"

	/* Create this as a global, so that spawns outside of tasks can
	 * refer to "PARENT".  (Ie: For the #define to work right...)
	 * Now used in place of TCM_RootNode() to provide a rather hideous
	 * mechanism for inserting external nodes (spawns outside of tasks)
	 * into the TCM Task Tree at locations other than the TCM_RootNode.
	 */
/* Definition for extern in tdl.H */
const TCM_Task_Tree_Ref & _TDL_ENCLOSING_TASK = TCM_RootNode();



/*static*/
u_int4 TDL::reportingLevel = 
#ifdef _TDL_REPORTING_LEVEL
				     _TDL_REPORTING_LEVEL ;
#else
				     TDL::EVERYTHING ;
#endif

/*static*/
BOOLEAN TDL::abortOnActionTaskMismatch = TRUE;

/*static*/
BOOLEAN TDL::closeLogStream = FALSE;

/*static*/
ostream * TDL::logStream    = & cerr;



	/* Returns TRUE for direct instances [SPAWN foo();],   *
	 * both Monitor and Monitor-child instances ["ACT-"],  *
	 * and SPAWN-with-WAIT macro instances ["-auto,wait"]. */
/*static*/ BOOLEAN
TDL::checkForTaskInstanceOf ( const char * theActualTaskName,
			      const char * theNameToCheck )
{
  if ( theActualTaskName == (const char *)NULL )
  {
    TDL::getLogStream()
      << "[TDL::checkForTaskInstanceOf]  Error:  theActualTaskName is NULL."
      << endl;
    return FALSE;
  }

  if ( theNameToCheck == (const char *)NULL )
  {
    TDL::getLogStream()
      << "[TDL::checkForTaskInstanceOf]  Error: theNameToCheck is NULL."
      << endl;
    return FALSE;
  }

  if ( StringEqual ( theActualTaskName, theNameToCheck ) == TRUE )
    return TRUE;

	/* Monitor child case -- Treat as instance of... */
  if (   ( strlen  ( theActualTaskName            ) >  4 )
      && ( strncmp ( theActualTaskName, "ACT-", 4 ) == 0 ) )
  {
    theActualTaskName += 4;

    if ( StringEqual ( theActualTaskName, theNameToCheck ) == TRUE )
      return TRUE;
  }

	/* Created through SpawnAndWait auto-generated function/macro case */
	/* theActualTaskName = theNameToCheck + "-auto,wait"               */
  if (   ( strlen ( theActualTaskName ) > strlen ( theNameToCheck )        )
      && ( strncmp ( theActualTaskName, theNameToCheck,
		     strlen ( theNameToCheck )                      ) == 0 )
      && ( strcmp  ( theActualTaskName + strlen ( theNameToCheck ),
		     "-auto,wait"                                   ) == 0 ) )
  {
    return TRUE;
  }

  return FALSE;
}





/*
 * Support for: (non-distributed version)
 *  extern  _TDL_Initialize_Return_Class TDL_Initialize(...);
 */
static BOOLEAN _TDL_HAS_TDL_INITIALIZE_ALREADY_RUN                  = FALSE;
static BOOLEAN _TDL_IS_TCM_LOGGING_CURRENTLY_ENABLED                = TRUE;
static int     _TDL_originalTcmTerminalLoggingOptions = 0;
static int     _TDL_originalTcmFileLoggingOptions     = 0;

_TDL_Initialize_Return_Class
TDL_Initialize ( /*BOOLEAN*/int theHideTcmMessages /* = TRUE */,
		 /*BOOLEAN*/int theHideTdlMessages /* = TRUE */ )
{
  if ( _TDL_HAS_TDL_INITIALIZE_ALREADY_RUN == FALSE )
  {
    TCM_Initialize();
    _TDL_HAS_TDL_INITIALIZE_ALREADY_RUN = TRUE;
  }

  if ( _TDL_IS_TCM_LOGGING_CURRENTLY_ENABLED == TRUE )
  {
    _TDL_originalTcmTerminalLoggingOptions = TCM_TerminalLoggingOptions();
    _TDL_originalTcmFileLoggingOptions     = TCM_FileLoggingOptions();
  }


  if ( theHideTcmMessages == FALSE )
  {
    TCM_SetTerminalLoggingOptions ( _TDL_originalTcmTerminalLoggingOptions );
    TCM_SetFileLoggingOptions     ( _TDL_originalTcmFileLoggingOptions     );
    _TDL_IS_TCM_LOGGING_CURRENTLY_ENABLED = TRUE;
  }
  else
  {
    TCM_SetTerminalLoggingOptions ( Log_None );
    TCM_SetFileLoggingOptions     ( Log_None );
    _TDL_IS_TCM_LOGGING_CURRENTLY_ENABLED = FALSE;
  }


  if ( theHideTdlMessages == FALSE )
  {
#ifdef _TDL_REPORTING_LEVEL
    TDL::setReportingLevel ( _TDL_REPORTING_LEVEL );
#else
    TDL::setReportingLevel ( TDL::EVERYTHING );
#endif
  }
  else
  {
    TDL::setReportingLevel ( TDL::EVERYTHING_BASIC );
  }


  return _TDL_Initialize_Return_Class();
}




/*
 * Originally intended to simplify testing of Distributed code,
 * this function may prove useful for developing Distributed
 * applications as well.
 */
void
TDL_ForkChildren (
	    void ( *     theFunctions []  )(u_int4),
	    const char * theNames [],
	    u_int4       theNumberOfFunctions,
	    MSecs        theDurationTimeInMilliSeconds,
	    MSecs        theTimeBetweenStartingProcessesInMilliseconds, /*=0*/
	    BOOLEAN      thePrintTimeStampsForDebugging /* = FALSE */  )
{
  int           * stdinfds       = new int     [ theNumberOfFunctions ];
  int           * stdouterrfds   = new int     [ theNumberOfFunctions ];
  pid_t         * childrenPids   = new pid_t   [ theNumberOfFunctions ];
  BOOLEAN       * childStillLive = new BOOLEAN [ theNumberOfFunctions ];

  const int4      killTimeoutConst    = 10 * MICROSECONDS_PER_SECOND;
  int4            childStillLiveCount = 0;
  BOOLEAN         initFailed          = FALSE;
  int4            lastFdRead          = -1;
  BOOLEAN         needsToStopChildren = TRUE;
  BOOLEAN         needsToKillChildren = TRUE;

  int             stdinPipeFds[2], stdouterrPipeFds[2];
  int             maximumFd;
  pid_t           pid;
  u_int4          i;
  int4            j;
  fd_set          readfds, writefds, exceptfds;
  struct timeval  currentTime, endTime, killTime, timeout;
  int             waitStatus;
  BOOLEAN         skippedNewline = TRUE; /* Value is reset before it's used. */

#define _TDL_READ_BUFFER_SIZE  MIN ( 1024, SSIZE_MAX )
  char            readBuffer [ _TDL_READ_BUFFER_SIZE ];
  ssize_t         readBytesCount;


	/* Idiocy check  -- INT_MAX == 24.855 days. */
  if ( theTimeBetweenStartingProcessesInMilliseconds > INT_MAX )
  {
    TDL::getLogStream()
      << "[TDL_ForkChildren]  Warning:  Value to large.  "
      << "theTimeBetweenStartingProcessesInMilliseconds was "
      << theTimeBetweenStartingProcessesInMilliseconds
      << ".  Limiting to " << INT_MAX << "." << endl;

    theTimeBetweenStartingProcessesInMilliseconds = INT_MAX;
  }


  for ( i = 0;  ( (i < theNumberOfFunctions) && (initFailed == FALSE) );  i++ )
  {
	/* If we temporally space these processes out,         *
	 * maybe the printed messages will be more consistent. */
    if ( i > 0 )
    {
      if ( theTimeBetweenStartingProcessesInMilliseconds / 1000 > 0 )
	sleep ( theTimeBetweenStartingProcessesInMilliseconds / 1000 );

      if ( theTimeBetweenStartingProcessesInMilliseconds % 1000 > 0 )
	usleep(( theTimeBetweenStartingProcessesInMilliseconds % 1000) * 1000);
    }


	/* Create pipes for our I/O. */
    if (   ( pipe ( stdinPipeFds     ) != 0 )
	|| ( pipe ( stdouterrPipeFds ) != 0 ) )
    {
      TDL::getLogStream()
	<< "[TDL_ForkChildren]  Error:  pipe() failed.  Errno = "
	<< errno << " (\"" << strerror(errno) << "\")." << endl;
      theNumberOfFunctions = i;
      initFailed = TRUE;
      break;
    }

    

	/* Fork off the child... */
    switch ( pid = fork() )
    {
	/* Fork failed. */
      case -1:
	TDL::getLogStream()
	  << "[TDL_ForkChildren]  Error:  fork() failed.  Errno = "
	  << errno << " (\"" << strerror(errno) << "\")." << endl;
	theNumberOfFunctions = i;
	initFailed = TRUE;
	break;


	/* Child case. */
      case 0:
	  /* Use cerr/cout -- NOT TDL::getLogStream() -- in child processes. */

		/* General cleanup... */ 
	if ( close ( stdinPipeFds[1] ) != 0 )
	  cerr << "[TDL_ForkChildren]  [child] Warning:  "
	       << "close(inPipe[1]) failed.  Errno = " << errno
	       << " (\"" << strerror(errno) << "\")." << endl;
	if ( close ( stdouterrPipeFds[0] ) != 0 )
	  cerr << "[TDL_ForkChildren]  [child] Warning:  "
	       << "close(outPipe[0]) failed.  Errno = " << errno
	       << " (\"" << strerror(errno) << "\")." << endl;


		/* Remap our primary Filedescriptors. */
	if ( close ( STDIN_FILENO ) != 0 )
	  cerr << "[TDL_ForkChildren]  [child] Warning:  "
	       << "close(stdin) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\")." << endl;
	if ( dup2 ( stdinPipeFds[0], STDIN_FILENO ) == -1 )
	  cerr << "[TDL_ForkChildren]  [child] Warning:  "
	       << "dup2(stdin) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\")." << endl;

	if ( close ( STDOUT_FILENO ) != 0 )
	  cerr << "[TDL_ForkChildren]  [child] Warning:  "
	       << "close(stdout) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\")." << endl;
	if ( dup2 ( stdouterrPipeFds[1], STDOUT_FILENO ) == -1 )
	{
	  cerr << "[TDL_ForkChildren]  [child] Error:  "
	       << "dup2(stdout) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\").   ABORTING..." << endl;
	  exit ( -1 );
	}

	if ( close ( STDERR_FILENO ) != 0 )
	{	/* How the heck do we print out these error messages? */
	  cout << "[TDL_ForkChildren]  [child] Warning:  "
	       << "close(stderr) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\")." << endl;
	  cerr << "[TDL_ForkChildren]  [child] Warning:  "
	       << "close(stderr) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\")." << endl;
	}
	if ( dup2 ( stdouterrPipeFds[1], STDERR_FILENO ) == -1 )
	{	/* How the heck do we print out these error messages? */
	  cout << "[TDL_ForkChildren]  [child] Warning:  "
	       << "dup2(stderr) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\").  ABORTING..." << endl;
	  cerr << "[TDL_ForkChildren]  [child] Warning:  "
	       << "dup2(stderr) failed.  " << "Errno = " << errno
	       << " (\"" << strerror(errno) << "\").  ABORTING..." << endl;
	  exit ( -1 );
	}


	    /* This would be a good thing to set here.         *
	     * (It should already be set for cerr.)            *
	     * It forces a flush after each output operation.  */
	cout.setf(ios::unitbuf);
	cerr.setf(ios::unitbuf);
	    /* And also make mixing both C++ and C code happy. */
#ifdef   _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD
	cout.setf(ios::stdio);
	cerr.setf(ios::stdio);
#else /* _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD */
	cout.sync_with_stdio(true);
	cerr.sync_with_stdio(true);
#endif /* _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD */

	    /* Run the function and we are done here. */
	theFunctions[i] ( i );
	exit(0);



	/* Parent case. */
      default:
	stdinfds       [ i ] = stdinPipeFds     [ 1 ];
	stdouterrfds   [ i ] = stdouterrPipeFds [ 0 ];
	childrenPids   [ i ] = pid;
	childStillLive [ i ] = TRUE;
	childStillLiveCount ++;

		/* General cleanup... */ 
	if ( close ( stdinPipeFds[0] ) != 0 )
	  TDL::getLogStream()
	    << "[TDL_ForkChildren]  Warning:  close(inPipe[0]) failed.  "
	    << "Errno = " << errno << " (\"" << strerror(errno) << "\")."
	    << endl;
	if ( close ( stdouterrPipeFds[1] ) != 0 )
	  TDL::getLogStream()
	    << "[TDL_ForkChildren]  Warning:  close(outerrPipe[1]) failed. "
	    << "Errno = " << errno << " (\"" << strerror(errno) << "\")."
	    << endl;
	break;

    } /* switch ( pid = fork() ) */
  } /* FOR ( 0 <= i < theNumberOfFunctions   && (initFailed == FALSE) )*/





	/********************************************************/
	/* Process input/output...                              */
	/* After theDurationTimeInMilliSeconds kill everything  */
	/* and wait for children to die.                        */
	/********************************************************/

	    /* This might be a good thing to set here too.     *
	     * (It should already be set for cerr.)            *
	     * It forces a flush after each output operation.  */
  cout.setf(ios::unitbuf);
  cerr.setf(ios::unitbuf);
	    /* And also make mixing both C++ and C code happy. */
#ifdef   _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD
  cout.setf(ios::stdio);
  cerr.setf(ios::stdio);
#else /* _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD */
  cout.sync_with_stdio(true);
  cerr.sync_with_stdio(true);
#endif /* _TDL_OLD_IOSTREAMS_INTERFACE_STANDARD */


  endTime = getTimeOfDay();

	/* If initFailed, endTime is NOW.  Otherwise add our waiting period. */
  if ( initFailed == FALSE )
    endTime += (theDurationTimeInMilliSeconds * 1000);

	/* Set our Kill-everything timeout... */
  killTime = endTime + killTimeoutConst;


  while ( childStillLiveCount > 0 )
  {
    currentTime = getTimeOfDay();
    
	/* IF we are still waiting, do a select on the fds. */
    if ( currentTime < endTime )
    {
      FD_ZERO ( & readfds   );
      FD_ZERO ( & writefds  );
      FD_ZERO ( & exceptfds );

      for ( maximumFd = 0, i = 0;  i < theNumberOfFunctions;  i++ )
      {
		/* Skip dead children */
	if ( childStillLive [ i ] != TRUE )
	  continue;

	FD_SET ( stdouterrfds [ i ], & readfds );
	if ( stdouterrfds [ i ] > maximumFd )
	  maximumFd = stdouterrfds [ i ];
      }

      timeout = endTime - currentTime;

      if ( select ( maximumFd + 1, &readfds, &writefds, &exceptfds, &timeout )
	   == -1 )
      {
	TDL::getLogStream()
	  << "[TDL_ForkChildren]  Warning:  select() failed.  Errno = "
	  << errno << " (\"" << strerror(errno) << "\")." << endl;
      }
    } /* if ( currentTime < endTime ) */


	/* Otherwise try to Stop or Kill the children... */
    else if (   (     needsToStopChildren == TRUE       )
	     || (   ( currentTime         >= killTime )
		 && ( needsToKillChildren == TRUE     ) ) )
    {
      if ( TDL::getIsReporting ( TDL::VERBOSE ) )
      {
	TDL::getLogStream()
	  << endl << endl << "[TDL_ForkChildren]  Now sending "
	  << ( (needsToStopChildren == TRUE) ? "SIGINT" : "SIGKILL" )
	  << " to all remaining children processes...";

	    /* Print the changed-fd header information. */
	lastFdRead = -1;
      }

      for ( i = 0;  i < theNumberOfFunctions;  i++ )
      {
		/* Skip dead children */
	if ( childStillLive [ i ] != TRUE )
	  continue;

	if ( kill ( childrenPids[i],
		    (needsToStopChildren == TRUE) ? SIGINT : SIGKILL ) != 0 )
	{
	  TDL::getLogStream()
	    << "[TDL_ForkChildren]  Error:  kill ( "
	    << childrenPids[i] << " [" << i << "], "
	    << ( (needsToStopChildren == TRUE) ? "SIGINT" : "SIGKILL" )
	    << " ) failed.   Errno = "
	    << errno << " (\"" << strerror(errno) << "\")." << endl;
	}
      } /* for ( i = 0;  i < theNumberOfFunctions;  i++ ) */

      if ( needsToStopChildren == TRUE )
	needsToStopChildren = FALSE;
      else
	needsToKillChildren = FALSE;

    } /* IF ( currentTime < endTime ) ... ELSE IF (Stop/Kill child time) ... */




	/* Read in whatever the children might have to say... */
    for ( i = 0;  i < theNumberOfFunctions;  i++ )
    {
	/* Ironically, here, We *don't* want to skip the dead children... */


      do
      {
	    /* Select again -- so we can handle multiple consecutive reads */
	FD_ZERO ( & readfds   );
	FD_ZERO ( & writefds  );
	FD_ZERO ( & exceptfds );

	FD_SET ( stdouterrfds[i], & readfds );

	timeout.tv_sec  = 0;
	timeout.tv_usec = 0;

	switch ( select ( stdouterrfds [ i ] + 1,
			  &readfds, &writefds, &exceptfds, &timeout) )
	{
	  case -1:
	    TDL::getLogStream()
	      << "[TDL_ForkChildren]  Warning:  "
	      << "[read] select() failed.  Errno = " << errno
	      << " (\"" << strerror(errno) << "\")." << endl;

	      /* NO BREAK!!! */

	  case 0:
	    readBytesCount = 0;
	    break;

	  default:
	    readBytesCount = read ( stdouterrfds[i],
				    readBuffer, _TDL_READ_BUFFER_SIZE );
	    break;
	} /* switch ( select ( stdouterrfds[i], ... ) ) */


	if ( readBytesCount > 0 )
	{
	  for ( j=0;  j < readBytesCount;  j++ )
	  {
		/* Did we change fd streams? */
	    if ( lastFdRead != stdouterrfds[i] )
	    {
		/* Skip a line to make things more readable */
	      cout << endl;
	      lastFdRead = stdouterrfds[i];
	      skippedNewline = TRUE;
	    }

 /* We want to avoid printing extra header lines, so we delay printing the
  * newline at the end of a buffer-read.
  * The truth table works out to:
  *   read[j]='\n'  skip   j+1 >= count  xPrint   newSkip
  *    T              T        T           1        T      (1)
  *    T              T        F           2        F      (1) (2)
  *    T              F        T           0        T
  *    T              F        F           1        F      (1)
  *
  *    F              T        T           1,c      F      (1)
  *    F              T        F           1,c      F      (1)
  *
  *    F              F        T           0,c      F     -------
  *    F              F        F           0,c      F     -------
  */
	    if (   ( readBuffer[j]  == '\n' )
		|| ( skippedNewline == TRUE ) )
	    {
		/* if skippedNewline != TRUE, then readBuffer[j]  == '\n' */

	          /* Print the header (where-its-from) string. */
	      if (   ( skippedNewline == TRUE )
		  || ( (j+1)          <  readBytesCount ) )
	      {
		cout << endl;
		if ( thePrintTimeStampsForDebugging )
		  timestamp ( cout );
		cout << theNames[i] << ":  ";
	      }

	      
		  /* "\n", "\n..." fencepost read case. */
	      if (   ( readBuffer[j]  == '\n'           )
		  && ( skippedNewline == TRUE           )
		  && ( (j+1)          <  readBytesCount ) )
	      {
		cout << endl;
		if ( thePrintTimeStampsForDebugging )
		  timestamp ( cout );
		cout << theNames[i] << ":  ";
	      }

		/* Did we just skip a newline? */
	      if (   ( readBuffer[j]  == '\n'            )
		  && ( (j+1)          >=  readBytesCount ) )
		skippedNewline = TRUE;
	      else
		skippedNewline = FALSE;
	    }
	    else
	    {
	      skippedNewline = FALSE;
	    }


	    if ( readBuffer[j] != '\n' )
	    {
	      cout << readBuffer[j];
	    }

	  } /* for ( j=0;  j < readBytesCount;  j++ ) */


		/* Run a minor sleep here, to permit task-switching,         *
		 * and improve our chances of continuing to read...          *
		 * (Assuming the user was interested in spacing things out.) */
	  if ( theTimeBetweenStartingProcessesInMilliseconds > 0 )
	    usleep ( 1000 );

	} /* if ( readBytesCount > 0 )*/

	else if (   ( readBytesCount == -1     )
		 && ( errno          != EAGAIN ) )
	{
	  TDL::getLogStream()
	    << "[TDL_ForkChildren]  Warning:  "
	    << "read ( stdouterr[" << i << "] ) failed.  Errno = "
	    << errno << " (\"" << strerror(errno) << "\")." << endl;
	}

      } while ( readBytesCount > 0 );


    } /* for ( i = 0;  i < theNumberOfFunctions;  i++ ) */



	/* Check and see if any of the children have finished yet... */
    for ( i = 0;  i < theNumberOfFunctions;  i++ )
    {
		/* Skip dead children */
      if ( childStillLive [ i ] != TRUE )
	continue;

      pid = waitpid ( childrenPids [ i ], & waitStatus, WNOHANG );
      if ( pid == -1 )
      {
	TDL::getLogStream()
	  << "[TDL_ForkChildren]  Warning:  "
	  << "waitpid ( " << childrenPids [ i ] << " [" << i
	  << "], ... ) failed.  Errno = " << errno
	  << " (\"" << strerror(errno) << "\")." << endl;
      }

      else if (   ( pid != 0                  )
	       && ( pid != childrenPids [ i ] ) )
      {
	TDL::getLogStream()
	  << "[TDL_ForkChildren]  Warning:  "
	  << "waitpid ( " << childrenPids [ i ] << " [" << i
	  << "], ... ) returned unexpected PID: " << pid
	  << ".  I wonder who that was..." << endl;
      }

      else if ( pid == childrenPids [ i ] )
      {
	childStillLive [ i ] = FALSE;
	childStillLiveCount --;
      }
    }


  } /* while ( childStillLiveCount > 0 ) */




	/**********************************/
	/* Final Cleanup...   All done... */
	/**********************************/
  delete [] stdinfds;
  delete [] stdouterrfds;
  delete [] childrenPids;
  delete [] childStillLive;

  cout << endl << endl;

#undef _TDL_READ_BUFFER_SIZE
} /* void TDL_ForkChildren ( ... ) */






static void
runCentral ( u_int4 theProcessIndex__akaJunk )
{
  MARKUSED ( theProcessIndex__akaJunk );

	/* Give us a mechanism to specify the path to central. */
  const char * centralPath = getenv ( "CENTRAL_PATH" );

	/* Otherwise, just pray it's on our current path... */
  if ( centralPath == (const char *)NULL )
    centralPath = "central";

  execlp ( centralPath, "central", "-u", NULL );

	/* Use cerr -- NOT TDL::getLogStream() -- in child processes. */
  cerr << "Error:  execlp ( \"" << centralPath << "\", \"central\", \"-u\" ) "
       << "FAILED!  Errno = " << errno << " (\"" << strerror(errno) << "\")."
       << endl;
}


/*
 * Convenience function to simplify  Central/Server/Client  case.
 */
void
TDL_ForkChildren (
		void ( * theServerFunction  )(u_int4),
		void ( * theClientFbunction )(u_int4),
		MSecs    theDurationTimeInMilliSeconds,
		MSecs    theTimeBetweenStartingProcessesInMilliseconds, /*=0*/
		BOOLEAN  thePrintTimeStampsForDebugging /* = FALSE */  )
{
  void ( * functions [3] )(u_int4);
  const char *  names[3];

  functions[0] = runCentral;
  names[0] = "central";

  functions[1] = theServerFunction;
  names[1] = " Server";

  functions[2] = theClientFbunction;
  names[2] = " Client";

  TDL_ForkChildren ( functions, names, 3,
		     theDurationTimeInMilliSeconds,
		     theTimeBetweenStartingProcessesInMilliseconds,
		     thePrintTimeStampsForDebugging );
}

