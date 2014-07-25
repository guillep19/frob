/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */

#include <tca.h>
#include "tcaInterface.h"

	/* Global static (singleton) CommInterfaceObject declaration */
CommInterface * CommInterface::commInterfaceObject = new TcaInterface();

#define SET_CONTEXT(index) \
 tcaSetContext((TCA_CONTEXT_PTR)getConnectionPtr((unsigned)index));

/*virtual*/ const char *
TcaInterface::_getRefName ( MESSAGE_REFERENCE  ref )
{
  return tcaReferenceName((TCA_REF_PTR)ref);
}

/*static*/ void
TcaInterface::sendQuery ( unsigned       theConnectionIndex,
			  const char * theMessageName,
			  void       * theData,
			  RAW_MESSAGE_HANDLER theHandler,
			  void       * theClientData        )
{
  SET_CONTEXT(theConnectionIndex);
  tcaQueryNotify( theMessageName, theData, (REPLY_HANDLER_FN)theHandler,
		  theClientData );
}

/*static*/ void
TcaInterface::_sendMessage ( unsigned     theConnectionIndex,
			     const char * theMessageName,
			     void       * theData        )
{
  SET_CONTEXT(theConnectionIndex);
  tcaBroadcast( theMessageName, theData );
}

/*static*/ int
TcaInterface::_receiveMessages ( struct timeval * theWaitingTime )

{
  SET_CONTEXT(0);  // Make sure you are listening to the main central
  return tcaHandleMessage((theWaitingTime == NULL ? 0
			   : (theWaitingTime->tv_sec +
			      theWaitingTime->tv_usec/1000000)));
}

CONNECTION_PTR TcaInterface::_connect (const char *moduleName,
				       const char *serverName)
{
  tcaConnectModule(moduleName, serverName);
  tcaWaitUntilReady();
  return (CONNECTION_PTR)tcaGetContext();
}

status_t TcaInterface::_disconnectAll (void)
{
  tcaClose();
  return SUCCESS;
}

// Unlike IPC, the TCA functions to do marshalling are not part of the API
extern "C" {
#include "../src/formatters.h"
CONST_FORMAT_PTR fmtFind(const char *name);
}

status_t TcaInterface::_marshallData ( const char *messageName, void *msgData,
				       void **buffer, unsigned *length)
{
  CONST_FORMAT_PTR format;

  format = fmtFind(messageName);
  if (format) {
    *length = bufferSize(format, msgData);
    *buffer = new char[*length];
    encodeData(format, msgData, *(char **)buffer, 0, *length);
    return SUCCESS;
  } else {
    return FAILURE;
  }
}

status_t TcaInterface::_unmarshallData ( const char *messageName, void *data,
					 void **msgData)
{
  UNUSED(messageName);

  // TCA already has unmarshalled the data (this is needed because IPC
  //  does not unmarshall by default, which is a bad thing and should be
  //  changed in the near future!)
  *msgData = data;
  return SUCCESS;
}
 
status_t TcaInterface::_freeData ( const char *messageName, void **msgData)
{
  tcaFreeData(messageName, *msgData);
  return SUCCESS;
}

void TcaInterface::_registerMessage  ( const char * theMessageName,
				       const char * theMessageFormat )
{
  SET_CONTEXT(0);  // Make sure you are registering just to the main central
  tcaRegisterBroadcastMessage(theMessageName, theMessageFormat);
}

void TcaInterface::_subscribeMessage ( const char         * theMessageName,
				       RAW_MESSAGE_HANDLER  theMessageHandler )
{
  SET_CONTEXT(0);  // Make sure you are subscribing just to the main central
  tcaRegisterHandler(theMessageName, theMessageName, theMessageHandler);
}
