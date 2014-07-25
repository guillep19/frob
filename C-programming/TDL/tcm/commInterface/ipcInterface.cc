/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */

#include <ipc.h>
#include "ipcInterface.h"

/* by default this isn't exposed */
extern IPC_VERBOSITY_TYPE ipcVerbosity;

	/* Global static (singleton) CommInterfaceObject declaration */
CommInterface * CommInterface::commInterfaceObject = new IpcInterface();

#define SET_CONTEXT(index) \
 IPC_setContext((IPC_CONTEXT_PTR)getConnectionPtr((unsigned)index));


/*virtual*/ const char *
IpcInterface::_defaultServerName() const
{
  const char * value = getenv ( "CENTRALHOST" );
  if ( value != (const char *)NULL )
    return value;
  else
    return CommInterface::_defaultServerName();
}

/*virtual*/ const char *
IpcInterface::_getRefName ( MESSAGE_REFERENCE  ref )
{
  return IPC_msgInstanceName((MSG_INSTANCE)ref);
}

/*static*/ void
IpcInterface::sendQuery ( unsigned       theConnectionIndex,
			  const char * theMessageName,
			  void       * theData,
			  RAW_MESSAGE_HANDLER theHandler,
			  void       * theClientData        )
{
  SET_CONTEXT(theConnectionIndex);
  IPC_queryNotifyData( theMessageName, theData, (HANDLER_TYPE)theHandler,
		       theClientData );
}

/*static*/ void
IpcInterface::_sendMessage ( unsigned       theConnectionIndex,
			     const char * theMessageName,
			     void       * theData        )
{
  SET_CONTEXT(theConnectionIndex);
  IPC_publishData( theMessageName, theData );
}

/*static*/ int
IpcInterface::_receiveMessages ( struct timeval * theWaitingTime )

{
  SET_CONTEXT(0);  // Make sure you are listening to the main central
  return IPC_listenClear((theWaitingTime == NULL ? 0
			  : (1000*theWaitingTime->tv_sec +
			     theWaitingTime->tv_usec/1000)));
}

CONNECTION_PTR IpcInterface::_connect (const char *moduleName,
				       const char *serverName)
{
  /* suppress errors about how we can't connect while we're
     in this loop */
  IPC_VERBOSITY_TYPE oldVerbosity = ipcVerbosity;
  IPC_setVerbosity(IPC_Silent);

  printf("Attempting to connect to IPC central server on %s...",
         (serverName ? serverName : "local host"));
  fflush(stdout);
  while (1) {
    /* arranging this loop without a sleep doesn't max out the processor
       as you might think, because IPC is internally blocking while
       waiting for a reply from the server */
    if (IPC_OK == IPC_connectModule(moduleName, serverName)) break;
    printf(".");
    fflush(stdout);
  }
  printf(" connected.\n");
  fflush(stdout);
  /* turn error reporting back up to the max */
  IPC_setVerbosity(oldVerbosity);

  return (CONNECTION_PTR)IPC_getContext();
}

status_t 
IpcInterface::_disconnectAll (void)
{
  IPC_disconnect();
  return SUCCESS;
}

status_t IpcInterface::_marshallData ( const char *messageName, void *data,
				       void **msgData, unsigned *length)
{
  IPC_VARCONTENT_TYPE varcontent;
  FORMATTER_PTR formatter = IPC_msgFormatter(messageName);

  if (formatter && IPC_marshall(formatter, data, &varcontent) == IPC_OK) {
    *msgData = varcontent.content;
    *length = varcontent.length;
    return SUCCESS;
  } else {
    return FAILURE;
  }
}

status_t IpcInterface::_unmarshallData ( const char *messageName, void *data,
					 void **msgData)
{
  FORMATTER_PTR formatter = IPC_msgFormatter(messageName);

  if (formatter && IPC_unmarshall(formatter, data, msgData) == IPC_OK) {
    return SUCCESS;
  } else {
    return FAILURE;
  }
}
 
status_t IpcInterface::_freeData ( const char *messageName, void **msgData)
{
  FORMATTER_PTR formatter = IPC_msgFormatter(messageName);

  if (formatter && IPC_freeData(formatter, *msgData) == IPC_OK) {
    return SUCCESS;
  } else {
    return FAILURE;
  }
}

void IpcInterface::_registerMessage  ( const char * theMessageName,
				       const char * theMessageFormat )
{
  SET_CONTEXT(0);  // Make sure you are registering just to the main central
  IPC_defineMsg(theMessageName, IPC_VARIABLE_LENGTH, theMessageFormat);
}

void IpcInterface::_subscribeMessage ( const char         * theMessageName,
				       RAW_MESSAGE_HANDLER  theMessageHandler )
{
  SET_CONTEXT(0);  // Make sure you are subscribing just to the main central
  IPC_subscribeData(theMessageName, (HANDLER_TYPE)theMessageHandler, NULL);
}
