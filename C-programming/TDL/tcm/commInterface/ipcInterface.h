/*
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see tcm/LICENSE.TXT)
 */

#ifndef IPC_INTERFACE_H
#define IPC_INTERFACE_H

#include <commInterface.h>

class IpcInterface : public CommInterface
{
public:
  static void sendQuery ( unsigned       theConnectionIndex,
			  const char * theMessageName,
			  void       * theData,
			  RAW_MESSAGE_HANDLER theHandler,
			  void       * theClientData        );

  static void sendQuery ( const char * theMessageName,
			  void       * theData,
			  RAW_MESSAGE_HANDLER theHandler,
			  void       * theClientData   )
  { sendQuery(0, theMessageName, theData, theHandler, theClientData); }

protected: // Internal, virtual functions that are IPC specific
  virtual const char * _defaultServerName() const;

  virtual const char * _getRefName  ( MESSAGE_REFERENCE  ref );
  virtual void _sendMessage ( unsigned       theConnectionIndex,
			      const char * theMessageName,
			      void       * theData        );
  virtual int _receiveMessages ( struct timeval * theWaitingTime );
  virtual CONNECTION_PTR _connect (const char *moduleName,
				   const char *serverName);
  virtual status_t _disconnectAll (void);

  virtual status_t _marshallData ( const char *messageName, void *data,
				   void **buffer, unsigned *length);
  virtual status_t _unmarshallData ( const char *messageName, void *buffer,
				     void **msgData);
  virtual status_t _freeData ( const char *messageName, void **msgData);

  virtual void _registerMessage  ( const char * theMessageName,
				   const char * theMessageFormat );
  virtual void _subscribeMessage ( const char         * theMessageName,
				   RAW_MESSAGE_HANDLER  theMessageHandler );
};

#endif /* IPC_INTERFACE_H */
