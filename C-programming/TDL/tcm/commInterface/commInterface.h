/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */

#ifndef COMM_INTERFACE_H
#define COMM_INTERFACE_H

#include <sys/time.h>
#include "tcm.h"
#include "../src/tcmList.h"

#ifndef HAS_STATUS
enum status_t
{
  FAILURE = FALSE,
  SUCCESS = TRUE
};
#define HAS_STATUS
#endif

	/* The REPLY_KEY is used to match responses with requests, for
	 * determining which reply-handler should be invoked.
	 * The default test (function "matches") is strict equality.
	 */
typedef void * REPLY_KEY;


	/* MESSAGE_HANDLER is a callback function. */
typedef BOOLEAN ( * MESSAGE_HANDLER )
				( void                    * theData,
				  const TCM_Task_Tree_Ref & theEnclosingTask,
				  void                    * theClientData );

	/* MESSAGE_HANDLER_PREDECLARATION can be used to pre-declare
	 * a MESSAGE_HANDLER callback function
	 */
typedef BOOLEAN ( MESSAGE_HANDLER_PREDECLARATION )
				( void                    * theData,
				  const TCM_Task_Tree_Ref & theEnclosingTask,
				  void                    * theClientData );


// These incomplete types are refined for the individual messaging systems.
typedef void *CONNECTION_PTR;
typedef void *MESSAGE_REFERENCE;

	/* RAW_MESSAGE_HANDLER is what the underlying communication system
	   calls when it receives a message. */
typedef void ( * RAW_MESSAGE_HANDLER ) ( MESSAGE_REFERENCE   theReference,
					 void              * theData );


typedef tcmList<class PendingReply> PendingList;

	/* Class CommInterface is used primarily as a namespace and for
	 * storing callbacks (PendingReply's) to messages.
	 * and secondarily as a _TDL_ArrayList of connected Contexts.
	 */
class CommInterface : private tcmList<class CommConnection>
{
public:
  enum CONSTANTS
  {
    INVALID_INDEX  = -1
  };

private:
  static CommInterface * commInterfaceObject;

public:
  static BOOLEAN  getCommHasBeenInitialized()
	    { return commInterfaceObject->hasBeenInitialized; }
  static BOOLEAN  getCommHasBeenClosedDown()
	    { return commInterfaceObject->hasBeenClosedDown;  }

  static status_t initializeComm ( const char * theModuleName,
				   const char * theServerName,
				   void (*registrationFn)());

  static status_t openConnection ( const char * theModuleName,
				   const char * theServerName);

  static const char * getModuleName ( void )
    { return commInterfaceObject->moduleName; }

  static unsigned        getConnectionCount()
                         { return commInterfaceObject->getLength(); }
  static int             getConnectionIndex ( const char * theConnectionName );

  static CONNECTION_PTR  getConnectionPtr  ( unsigned theConnectionIndex );
  static CONNECTION_PTR  getConnectionPtr  ( const char * theConnectionName );
  
  static const char * getRefName   ( MESSAGE_REFERENCE  ref )
    { return commInterfaceObject->_getRefName(ref); }

  static PendingList &     getPendingReplies()
    { return commInterfaceObject->pendingReplies;  }

	/**************************************/
	/* Message Registration methods... */
	/**************************************/

  static void registerMessage ( const char * theMessageName,
				const char * theMessageFormat )
    { commInterfaceObject->_registerMessage(theMessageName, theMessageFormat);}

  static void subscribeMessage ( const char         * theMessageName,
				 RAW_MESSAGE_HANDLER  theMessageHandler )
    { commInterfaceObject->_subscribeMessage(theMessageName, 
					     theMessageHandler); }

        /***************************/
	/* General control methods */
        /***************************/

  static void          sendMessage ( unsigned     theConnectionIndex,
				     const char * theMessageName,
				     void       * theData        )
    { commInterfaceObject->_sendMessage(theConnectionIndex, theMessageName, 
					theData); }

  static void          sendMessage ( const char * theMessageName,
				     void       * theData        )
    { sendMessage(0, theMessageName, theData); }


  static int      receiveMessages( void ) 
    { return commInterfaceObject->_receiveMessages(NULL); }
  static int      receiveMessages ( struct timeval * theWaitingTime )
    { return commInterfaceObject->_receiveMessages(theWaitingTime); }

  static status_t closeAllConnections ();

  static void     processHandler ( REPLY_KEY         theReplyKey,
				   MESSAGE_REFERENCE theReference,
				   void            * theData );

	/**************************************/
	/* Query Register-callback methods... */
	/**************************************/

  static void addPendingReply( REPLY_KEY                 theReplyKey,
			       MESSAGE_HANDLER           theMessageHandler,
			       const TCM_Task_Tree_Ref & theTaskTreeRef,
			       void                    * theClientData );

  static void removePendingReply( REPLY_KEY theReplyKey );

  static void eventHandlerForTCM (BOOLEAN (* tcmDispatchExternalEventsHandler)
				             ( void           * theTcmData,
					       struct timeval * theTimeout ),
				   void           * theTcmData,
				   struct timeval * theTimeout,
				   BOOLEAN          theHasTcmExternalEvents  );


	/**************************************/
	/* Data Handling methods... */
	/**************************************/

  static status_t marshallData ( const char *messageName, void *msgData,
				 void **buffer, unsigned *length)
    { return commInterfaceObject->_marshallData(messageName, msgData, 
						buffer, length); }
  static status_t marshallData ( MESSAGE_REFERENCE ref, void *msgData,
				 void **buffer, unsigned *length)
    { return CommInterface::marshallData(getRefName(ref), msgData,
					 buffer, length); }

  static status_t unmarshallData ( const char *messageName, void *buffer,
				   void **msgData)
    { return commInterfaceObject->_unmarshallData(messageName, buffer, 
						  msgData); }
  static status_t unmarshallData ( MESSAGE_REFERENCE ref, void *buffer,
				   void **msgData)
    { return CommInterface::unmarshallData(getRefName(ref), buffer, msgData); }

  static status_t freeData ( const char *messageName, void **msgData)
    { return commInterfaceObject->_freeData(messageName, msgData); }
  static status_t freeData ( MESSAGE_REFERENCE ref, void **msgData)
    { return CommInterface::freeData(getRefName(ref), msgData); }

        /**********************************/
	/* Actual instance data & methods */
        /**********************************/

protected:
  BOOLEAN     hasBeenInitialized;
  BOOLEAN     hasBeenClosedDown;
  PendingList pendingReplies;
  unsigned    maxIndex;
  const char *moduleName;

  CommInterface() : 
    hasBeenInitialized(FALSE), hasBeenClosedDown(FALSE), maxIndex(0) {}

  CommConnection *getCommConnection (unsigned index);
  CommConnection *getCommConnection (const char *name);

public:
  virtual ~CommInterface();  /* Does a closeAllConnections() */

protected: // Internal, virtual functions that are messaging-system specific
  virtual const char * _defaultServerName() const;

  virtual const char * _getRefName  ( MESSAGE_REFERENCE  ref ) = 0;
  virtual void _sendMessage ( unsigned     theConnectionIndex,
			      const char * theMessageName,
			      void       * theData        ) = 0;
  virtual int _receiveMessages ( struct timeval * theWaitingTime ) = 0;
  virtual CONNECTION_PTR _connect (const char *moduleName,
				   const char *serverName) = 0;
  virtual status_t _disconnectAll (void) = 0;

  virtual status_t _marshallData ( const char * messageName, void *msgData,
				   void **buffer, unsigned *length) = 0;
  virtual status_t _unmarshallData ( const char * messageName, void *buffer,
				     void **msgData) = 0;
  virtual status_t _freeData ( const char * messageName, void **msgData) = 0;

  virtual void _registerMessage  ( const char * theMessageName,
				   const char * theMessageFormat ) = 0;
  virtual void _subscribeMessage ( const char         * theMessageName,
				   RAW_MESSAGE_HANDLER  theMessageHandler) = 0;
};

#endif /* COMM_INTERFACE_H */
