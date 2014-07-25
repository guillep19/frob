/**************************************************************************
 * 
 * PROJECT: Task Control Management (TCM)
 *
 * FILE: external.cc
 *
 * ABSTRACT: Defines functions for dealing with external events.
 *
 * EXPORTS:
 *
 * $Revision: 1.9 $
 * $Date: 2009/01/15 17:00:04 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 *
 * REVISION HISTORY:
 *
 * $Log: external.cc,v $
 * Revision 1.9  2009/01/15 17:00:04  reids
 * Released under simplified BSD open source license
 *
 * Revision 1.8  2002/12/23 02:24:55  da0g
 * External_Events::dispatchEvents() now sets the system thread's task
 *  to TCM_RootNode(), overriding any user-specified (Resume) values.
 *
 * Revision 1.7  2002/09/16 22:47:58  da0g
 * Updated comments as part of other work.
 *
 * Revision 1.6  2001/10/23 22:52:58  da0g
 * Added Threading support.  Cached lastchild.
 *
 * Revision 1.5  2001/03/13 03:33:43  reids
 * Mistake in the RedHat 6 version
 *
 * Revision 1.4  2001/03/02 15:32:59  reids
 * Fixed up the previous millisecond timing fix
 *
 * Revision 1.3  2001/03/02 12:51:50  reids
 * Made millisecond-level timing much more accurate, especially under RH5.2
 *
// Revision 1.2  98/10/30  11:16:32  da0g
// Added ExternalEventBypassHandler.
// 
 * Revision 1.1  1997/12/29 17:06:18  reids
 * Version that has the basic functionality needed to support TDL.
 *
 **************************************************************************/

#ifndef macintosh
#include <errno.h>
#else
#include <sys/errno.h>
#endif
#include "tcmLogging.h"
#include "external.h"
#ifdef macintosh
#include "agenda.h"
#endif
#include "tcmThread.h"


BOOLEAN runDispatchEvents ( void * theExternalEvents,
			    struct timeval * timeout )
{
  return ((External_Events *) theExternalEvents) -> _dispatchEvents( timeout );
}


/**************************************************************************
 *
 * CLASS: External Events
 *
 **************************************************************************/

void External_Events::dispatchEvents (struct timeval *timeout)
{
  	/* Push main thread's current Task_Tree_Node to be TCM_RootNode(). */
  Task_Tree_Ref previousTaskTreeRef
    = setMainThreadCurrentSystemTaskTreeRef ( GET_TCM_GLOBAL(rootNode) );

  	/* Override any user-specified Task-Thread mappings, if necessary */
  TCM_PushUserTaskForThisThreadForDurationOfObject
    ifNecessaryOverrideUserTaskThreadMappingWithSystemDefault;

	/* Do external events... */
  if ( getHasBypassHandler() )
    ( * getBypassHandler() ) ( & runDispatchEvents,
			       (void *)this,
			       timeout,
			       getHasDispatchableEvents() );
  else
    _dispatchEvents ( timeout );

	/*Cancel override of user-specified Task-Thread mapping, if necessary*/
  ifNecessaryOverrideUserTaskThreadMappingWithSystemDefault . cleanup();

	/* Pop main thread's current Task_Tree_Node back to what it was. */
  setMainThreadCurrentSystemTaskTreeRef ( previousTaskTreeRef );
}


BOOLEAN External_Events::_dispatchEvents (struct timeval *timeout)
{
  fd_set readMask;
  struct timeval aTimeout, * aTimeoutPtr;
  int    ret;
  
  do {
	/* If select fails, readMask & timeout could be undefined. */
    readMask = _eventMask;

    if ( timeout != (struct timeval *)NULL )
    {
      aTimeout . tv_sec  = timeout -> tv_sec;
      aTimeout . tv_usec = timeout -> tv_usec;
      aTimeoutPtr = & aTimeout;

      // NOTE: Linux seems to round timeouts up to the nearest jiffy (10 msecs)
      // RH 5.2 seems to have a bug in that it rounds up even further
      // (another 10 msecs).  This code tries to reduce the error, somewhat.
#ifdef REDHAT_52
      // Set to zero if requested timeout is pretty small
      if (aTimeout.tv_sec == 0 && aTimeout.tv_usec <= 2500) {
	aTimeout.tv_usec = 0;
      }
#ifndef /*NOT*/ REDHAT_6
      else {
	if (aTimeout.tv_usec >= 10000) {
	  aTimeout.tv_usec -= 10000;
	} else if (aTimeout.tv_sec > 0) {
	  aTimeout.tv_sec--; aTimeout.tv_usec += 1000000 - 10000;
	}
	// Round up explicitly to the next even multiple of 10 msecs.
	aTimeout.tv_usec = 10000*((aTimeout.tv_usec+9999)/10000);
      }
#endif /*NOT REDHAT_6*/
#endif /*REDHAT_52*/
    }
    else
    {
      aTimeoutPtr = (struct timeval *)NULL;
    }

	/* If we are threaded, fully unlock ourself before waiting... */
    unsigned int numberOfLocks
      = TCM_FullyUnlockMasterMutexReturningNumberOfLocks();

    ret = select ( _maxConnection+1, &readMask, NULL, NULL, aTimeoutPtr );

	/* If we are threaded, fully re-lock ourself afterwards... */
    TCM_FullyRelockMasterMutexNumberOfLocksTimes ( numberOfLocks );

  } while (ret < 0 && errno == EINTR);
  
	/* select *might* change the timeout value. */
  if ( timeout != (struct timeval *)NULL )
  {
    timeout -> tv_sec  = aTimeout . tv_sec;
    timeout -> tv_usec = aTimeout . tv_usec;
  }

  if ( ret == 0 )
  { // Timeout occurred
    return FALSE;
  }
  else if ( ret < 0 )
  {
    tcmError("dispatchEvents: Error on select (%d)\n", errno);
    return FALSE;
  }
  else
  {
    for ( int i=0;  i <= _maxConnection; i++ )
    {
      if ( FD_ISSET ( i, & readMask ) )
      {
        _find ( i ) -> invoke();
      }
    }
    return TRUE;
  }
}

void External_Events::addExternalEvent (int sd, SD_CALLBACK_FN_TYPE callback,
					const void *clientData)
{
	External_Event externalEvent(sd, callback, clientData);
	
	if (_remove(sd)) 
	  tcmWarning("addExternalEvent: Replacing existing external event handler for sd %d\n", sd);
	
	_eventList.insertLast(externalEvent);
	FD_SET(sd, &_eventMask);
	if (sd > _maxConnection) _maxConnection = sd;
}
 
void External_Events::removeExternalEvent (int sd)
{	
	if (_remove(sd)) {
		FD_CLR(sd, &_eventMask);
		if (sd == _maxConnection) {
		  // Reset the "maxConnection" value
		  _maxConnection = 0;
			for (int i=sd-1; i>0; i--) {
			  if (FD_ISSET(i, &_eventMask)) {
			  	_maxConnection = i;
			  	break;
			  }
			}
		}
	} else {
	  tcmWarning("removeExternalEvent: No existing external event handler for sd %d\n", sd);
	}
}

External_Event * External_Events::_find (int sd) const
{
	Const_External_Event_Iterate externIter(_eventList);
	External_Event externEvent(sd);
	
	for (; externIter; externIter++) {
		if (*externIter() == externEvent) {
			return externIter();
    }
  }
  return NULL;
}

 
BOOLEAN External_Events::_remove (int sd)
{
	External_Event *externalEvent = _find(sd);
	
	if (externalEvent) {
	  _eventList.removeItem(*externalEvent);
	}
  return externalEvent != NULL;
}
