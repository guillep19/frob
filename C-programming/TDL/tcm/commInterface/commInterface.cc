/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */

/*
 * Under gcc 3.2.3, to discourage the use of the now obsolete iostream.h
 * interface, we are treated to warning messages during compilation.
 * However, rewriting the code will break compatibility with older systems,
 * and the iostream.h file still provides the precise interface required.
 * So, for now, we are just disabling these messages.
 */
#define _CPP_BACKWARD_BACKWARD_WARNING_H
#include <iostream>
using namespace std;
#include "commInterface.h"

#include "../src/tcmLogging.h"
// Since all the list functions are template functions, need to include
// the CC file here, so the right template functions can be generated.
#include "../src/tcmList.cc"

template <class T> tcmList<T> tcmList<T>::_freeList;
template <class T> T tcmList<T>::nullItem;

	/* PendingReply is a glorified structure that is strictly internal
	 * to this file.  It stores the necessary data to achieve
	 * extended callback functionality for TDLC "POSTPONED" Tasks.
	 */
class PendingReply
{
protected:
  REPLY_KEY        pendingHandlerType;
  MESSAGE_HANDLER   messageHandler;

	/* Note:  These next two will increment & decrement  *
	 * the ref-counts so as to avoid premature deletion. */
  TCM_Task_Tree_Ref   correspondingTaskTreeRef;
  void              * clientData;


public:
  PendingReply () {};

  PendingReply ( REPLY_KEY                 theReplyKey,
		 MESSAGE_HANDLER           theMessageHandler,
		 const TCM_Task_Tree_Ref & theCorrespondingTaskTreeRef,
		 void                    * theClientData )
    :  pendingHandlerType       ( theReplyKey       ),
       messageHandler           ( theMessageHandler           ),
       correspondingTaskTreeRef ( theCorrespondingTaskTreeRef ),
       clientData               ( theClientData               )
  {}

  virtual ~PendingReply();

  BOOLEAN matches ( REPLY_KEY   theMessageType )
  {
    return ( theMessageType == pendingHandlerType ) ? TRUE : FALSE;
  }

  BOOLEAN operator== ( const PendingReply &thePendingReply)
  {
    return ( thePendingReply.pendingHandlerType == pendingHandlerType );
  }

  BOOLEAN process ( void * theData )
  {
	/* Invoke the callback */
    return ( * messageHandler ) ( theData, correspondingTaskTreeRef,
				  clientData );
  }
};

PendingReply::~PendingReply()
{
	/* This should not really be necessary,                           *
	 * but its not paranoia if the compiler is really out to get you. */
  pendingHandlerType       = REPLY_KEY ( NULL );
  messageHandler           = MESSAGE_HANDLER   ( NULL );
  correspondingTaskTreeRef = TCM_Task_Tree_Ref();
  clientData               = NULL;
}

	/* CommConnection is a glorified structure that is strictly internal
	 * to this file.  It manages the data associated with individual
	 * server and/or module connections
	 */
class CommConnection
{
protected:
  unsigned         index;
  const char     * name;
  CONNECTION_PTR   connection;

public:
  CommConnection ( ) : index(0), name ( NULL ), connection ( NULL) {}
  CommConnection ( unsigned       index,
		   const char   * name,
		   CONNECTION_PTR connection)
    : index(index), name ( name ), connection ( connection) {}

  virtual ~CommConnection();

  const char      * getConnectionName() { return name; }
  unsigned          getConnectionIndex() { return index; }
  CONNECTION_PTR  & getConnection() { return connection; }

  void setConnectionIndex (unsigned theIndex) { index = theIndex; }
};

/*virtual*/
CommConnection::~CommConnection()
{
  name = (const char *) NULL;
  connection = CONNECTION_PTR ( NULL );
}

CommConnection * CommInterface::getCommConnection (unsigned index)
{
  List_Iterate<CommConnection> listIter(this);

  for ( ; listIter; listIter++) {
    if (index == listIter()->getConnectionIndex())
      return listIter();
  }
  return NULL;
}

CommConnection * CommInterface::getCommConnection (const char *name)
{
  List_Iterate<CommConnection> listIter(this);

  for ( ; listIter; listIter++) {
    if (!strcmp(name, listIter()->getConnectionName()))
      return listIter();
  }
  return NULL;
}

/*virtual*/
CommInterface::~CommInterface()
{
  if (   ( CommInterface::getCommHasBeenInitialized() == TRUE  )
      && ( CommInterface::getCommHasBeenClosedDown()  == FALSE ) )
  {
    CommInterface::closeAllConnections();
  }

  clear();
}


/*static*/ status_t
CommInterface::initializeComm ( const char * theModuleName,
				const char * theServerName,
				void (*registrationFn)())
{
  status_t status = CommInterface::openConnection(theModuleName, 
						  theServerName);

  if (status == SUCCESS && registrationFn != NULL) {
    (*registrationFn)();
  }
  return status;
}

/*static*/ status_t
CommInterface::openConnection ( const char * theModuleName,
				const char * theServerName)
{
  CONNECTION_PTR connection;
  unsigned index;

  if (theServerName == NULL) theServerName = "localhost";
  connection = commInterfaceObject->getCommConnection(theServerName);
  if (connection) {
    return SUCCESS;
  } else {
    commInterfaceObject->moduleName = theModuleName;
    connection = commInterfaceObject->_connect(theModuleName, theServerName);
    if (connection != NULL) {
      index = commInterfaceObject->maxIndex;
      commInterfaceObject->insertLast(CommConnection(index,  theServerName,
						     connection));
      commInterfaceObject->maxIndex++;
      commInterfaceObject->hasBeenInitialized = TRUE;
      commInterfaceObject->hasBeenClosedDown = FALSE;
      return SUCCESS;
    } else {
      cerr << "[CommInterface:openConnection]  Error:  Unable to open \""
	   << theModuleName << " at " << theServerName << endl;
      return FAILURE;
    }
  }
}

/*static*/ 
int CommInterface::getConnectionIndex ( const char * theConnectionName )
{
  CommConnection *commConnection;

  commConnection = commInterfaceObject->getCommConnection(theConnectionName);
  if (commConnection) {
    return commConnection->getConnectionIndex();
  } else {
#if 0
    cerr << "[CommInterface::getConnectionPtr]  Error:  Invalid name ("
	 << theConnectionName << ")" << endl;
#endif
    return -1;
  }
}

/*static*/ CONNECTION_PTR
CommInterface::getConnectionPtr ( unsigned theConnectionIndex )
{
  CommConnection *commConnection;

  commConnection = commInterfaceObject->getCommConnection(theConnectionIndex);
  if (commConnection) {
    return commConnection->getConnection();
  } else {
    cerr << "[CommInterface::getConnectionPtr]  Error:  Invalid index ("
	 << theConnectionIndex << ")" << endl;
    return CONNECTION_PTR ( NULL );
  }
}

/*static*/ status_t
CommInterface::closeAllConnections (void)
{
  if (   ( CommInterface::getCommHasBeenInitialized() == TRUE  )
      && ( CommInterface::getCommHasBeenClosedDown()  == FALSE ) )
  {
    commInterfaceObject->_disconnectAll();
    CommInterface::commInterfaceObject->hasBeenClosedDown = TRUE;
    return SUCCESS;
  }
 else
  {
    cerr << "[CommInterface:closeAllConnections]  ERROR:  "
	 << "Either NEVER initialized or already been closed!" << endl;
    return FAILURE;
  }
}

static PendingReply * getPendingReply( REPLY_KEY theReplyKey )
{
  List_Iterate<PendingReply> listIter(CommInterface::getPendingReplies());

  for ( ; listIter; listIter++) {
    if ( (listIter()) -> matches ( theReplyKey ) ) {
      return listIter();
    }
  }
  // Not found
  return NULL;
}

static void _removePendingReply( PendingReply *pendingReply )
{
  CommInterface::getPendingReplies().removeItem(*pendingReply);
}

/*static*/ void
CommInterface::processHandler ( REPLY_KEY         theReplyKey,
				MESSAGE_REFERENCE theReference,
				void            * theData )
{
  UNUSED(theReference);

  PendingReply *pendingReply;

  pendingReply = getPendingReply(theReplyKey);
  if ( pendingReply ) {

    if ( ( pendingReply -> process ( theData ) ) == TRUE )
      {
	/* Only remove this snode from the list if process() returns TRUE,
	 * since some callbacks may need to be invoked multiple times
	 * before they are done...
	 */
	_removePendingReply(pendingReply);
      } /* IF ( this snode process()'s theData successfully ) */

    /* If we found a matches() snode, we should look no further... */
    return;

  }

  cerr << "[CommInterface:processMessage]  Warning:  "
       << "No matching message-node found for message type # "
       << ((void *) theReplyKey) << "." << endl;
}

/*static*/ void
CommInterface::addPendingReply( REPLY_KEY                 theReplyKey,
				MESSAGE_HANDLER           theMessageHandler,
				const TCM_Task_Tree_Ref & theTaskTreeRef,
				void                    * theClientData )
{
  commInterfaceObject->getPendingReplies()
                      . insertLast ( PendingReply ( theReplyKey,
						    theMessageHandler,
						    theTaskTreeRef,
						    theClientData ) );
}

/*static*/ void
CommInterface::removePendingReply( REPLY_KEY theReplyKey )
{
  PendingReply *pendingReply;

  pendingReply = getPendingReply(theReplyKey);
  if (pendingReply) {
    _removePendingReply(pendingReply);
  } else {
    cerr << "[CommInterface:removePendingReply]  Warning:  "
       << "No matching message-node found for message type # "
       << ((void *) theReplyKey) << "." << endl;
  }
}

/*static*/ void
CommInterface::eventHandlerForTCM (BOOLEAN (* tcmDispatchExternalEventsHandler)
				           ( void           * theTcmData,
					     struct timeval * theTimeout ),
				   void           * theTcmData,
				   struct timeval * theTimeout,
				   BOOLEAN          theHasTcmExternalEvents  )
{
#define POLL_TIME_INCREMENT  1
  struct timeval  aTimeout, realTimeout, counter;
  BOOLEAN         hasTimeout  = FALSE;
  BOOLEAN         readyToExit = FALSE;

	/* Deal with idiocy... */
  if ( theTimeout != (struct timeval *) NULL )
  {
    hasTimeout = TRUE;
    realTimeout.tv_sec  = theTimeout->tv_sec;
    realTimeout.tv_usec = theTimeout->tv_usec;

    if ( realTimeout.tv_usec > 1000000 )
    {
      realTimeout.tv_sec  += realTimeout.tv_usec / 1000000;
      realTimeout.tv_usec  = realTimeout.tv_usec % 1000000;
    }
  }

	/* Do we need fancy polling? */
  if ( theHasTcmExternalEvents == TRUE )
  {
    for ( counter.tv_sec = POLL_TIME_INCREMENT
	    ;
	  (   (   ( hasTimeout     == FALSE              )
	       || ( counter.tv_sec <= realTimeout.tv_sec ) )
	   && (     readyToExit    == FALSE                ) )
	    ;
	  counter.tv_sec += POLL_TIME_INCREMENT )
      {
	/* Receive the current message... */
	aTimeout.tv_usec = 0;
	aTimeout.tv_sec  = POLL_TIME_INCREMENT;
	if ( CommInterface::receiveMessages ( &aTimeout ) != -1 )
	{
	  readyToExit = TRUE;
	}

	/* Handle TCM external events */
	aTimeout.tv_usec = 0;
	aTimeout.tv_sec  = 0;
	if ( (* tcmDispatchExternalEventsHandler) ( theTcmData, & aTimeout )
	     == TRUE )
	{
	  readyToExit = TRUE;
	}
      } /* FOR ( POLL_TIME_INCREMENT <= counter.tv_sec <= realTimeout.tv_sec)*/

    /* Do any remaining time... */
    if (   ( hasTimeout  == TRUE  )
	&& ( readyToExit == FALSE ) )
    {
	/* Receive the current TCX messages... */
      aTimeout.tv_sec  = realTimeout.tv_sec % POLL_TIME_INCREMENT;
      aTimeout.tv_usec = realTimeout.tv_usec;
      CommInterface::receiveMessages ( &aTimeout );

	/* Handle TCM external events */
      aTimeout.tv_sec  = 0;
      aTimeout.tv_usec = 0;
      (* tcmDispatchExternalEventsHandler) ( theTcmData, & aTimeout );      
    }

  }
  else  /* IE: theHasTcmExternalEvents == FALSE */
  {
    if ( hasTimeout != TRUE )
    {
      //Wait a short time -- to handle signals that may get sent in other ways.
      realTimeout.tv_sec = 0;
      realTimeout.tv_usec = 50000;
    }
    CommInterface::receiveMessages(&realTimeout);
  }

	/* Flush all currently received messages before returning */
  CommInterface::receiveMessages();
}


/*virtual*/ const char *
CommInterface::_defaultServerName() const
{
  return "localhost";
}
