/******************************************************************************
 * PROJECT: IPC (Interprocess Communication) Package
 *
 * (c) Copyright 2011 Reid Simmons.  All rights reserved.
 *
 * FILE: ipcLisp.c
 *
 * ABSTRACT: C-code for interfacing specifically with Lisp
 *           Used by SWIG (see ffi/IPC.i)
 *
 *       $Id: ipcLisp.c,v 2.1 2013/07/31 19:54:49 reids Exp $
 * $Revision: 2.1 $
 *     $Date: 2013/07/31 19:54:49 $
 *   $Author: reids $
 *    $State: Exp $
 *   $Locker:  $
 *
 * Copyright (c) 2011, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * REVISION HISTORY
 * $Log: ipcLisp.c,v $
 * Revision 2.1  2013/07/31 19:54:49  reids
 * Updated for using SWIG
 *
 *
 ****************************************************************/	

#include "ipcLisp.h"
#include "lisp.h"
extern long lisp_value(int index);
extern void *lisp_call_address(int index);

int _msgHandlerIndex = 0;
int _queryHandlerIndex = 0;
int _fdHandlerIndex = 0;
int _connectHandlerIndex = 0;
int _disconnectHandlerIndex = 0;
int _changeHandlerIndex = 0;
int _timerHandlerIndex = 0;

void registerIndices (int msgHandlerIndex, int queryHandlerIndex,
		      int fdHandlerIndex, int connectHandlerIndex,
		      int disconnectHandlerIndex, int changeHandlerIndex,
		      int timerHandlerIndex)
{
  _msgHandlerIndex = msgHandlerIndex;
  _queryHandlerIndex = queryHandlerIndex;
  _fdHandlerIndex = fdHandlerIndex;
  _connectHandlerIndex = connectHandlerIndex;
  _disconnectHandlerIndex = disconnectHandlerIndex;
  _changeHandlerIndex = changeHandlerIndex;
  _timerHandlerIndex = timerHandlerIndex;
}

static void ipcLispMsgHandler (MSG_INSTANCE msg, void *data, void *key)
{
#if 0
  fprintf(stderr, "Calling ipcLispMsgHandler for message %s (%d)\n", 
	  IPC_msgInstanceName(msg), (int)key);
#endif
  void (*lispMsgHandler)(MSG_INSTANCE msg, void *data, int key);
  lispMsgHandler = lisp_call_address(_msgHandlerIndex);
  (*lispMsgHandler)(msg, data, (int)(size_t)key);
}

IPC_RETURN_TYPE subscribe (const char *msgName, const char *hndName, long key)
{
  return _IPC_subscribe(msgName, hndName, ipcLispMsgHandler,
                        (void *)(size_t)key, 0);
}

static void ipcLispFdHandler (int fd, void *key)
{
#if 0
  fprintf(stderr, "Calling ipcLispFdHandler for file descriptor %d\n", fd);
#endif
  void (*fdMsgHandler)(int fd, int key);
  fdMsgHandler = lisp_call_address(_fdHandlerIndex);
  (*fdMsgHandler)(fd, (int)(size_t)key);
}

IPC_RETURN_TYPE subscribeFD (int fd, long key)
{
  return IPC_subscribeFD(fd, ipcLispFdHandler, (void *)(size_t)key);
}

IPC_RETURN_TYPE unsubscribeFD (int fd)
{
  return IPC_unsubscribeFD(fd, ipcLispFdHandler);
}

static void ipcLispConnectHandler (const char *moduleName, void *clientData)
{
#if 0
  fprintf(stderr, "Calling ipcLispConnectHandler for module %s\n", moduleName);
#endif
  void (*connectHandler)(const char *moduleName);
  connectHandler = lisp_call_address(_connectHandlerIndex);
  (*connectHandler)(moduleName);
}

static void ipcLispDisconnectHandler (const char *moduleName,
				      void *clientData)
{
#if 0
  fprintf(stderr, "Calling ipcLispDisconnectHandler for module %s\n",
	  moduleName);
#endif
  void (*disconnectHandler)(const char *moduleName);
  disconnectHandler = lisp_call_address(_disconnectHandlerIndex);
  (*disconnectHandler)(moduleName);
}

IPC_RETURN_TYPE subscribeConnect (void)
{
  return IPC_subscribeConnect(ipcLispConnectHandler, NULL);
}

IPC_RETURN_TYPE subscribeDisconnect (void)
{
  return IPC_subscribeDisconnect(ipcLispDisconnectHandler, NULL);
}

IPC_RETURN_TYPE unsubscribeConnect (void)
{
  return IPC_unsubscribeConnect(ipcLispConnectHandler);
}

IPC_RETURN_TYPE unsubscribeDisconnect (void)
{
  return IPC_unsubscribeDisconnect(ipcLispDisconnectHandler);
}

static void ipcLispChangeHandler (const char *msgName, int numHandlers,
                                    void *clientData)
{
#if 0
  fprintf(stderr, "Calling ipcLispChangeHandler for message %s\n", msgName);
#endif
  void (*changeHandler)(const char *moduleName, int numHandlers);
  changeHandler = lisp_call_address(_changeHandlerIndex);
  (*changeHandler)(msgName, numHandlers);
}

static void ipcLispQueryHandler (MSG_INSTANCE msg, void *data, void *qhndKey)
{
#if 0
  fprintf(stderr, "Calling ipcLispQueryHandler for message %s (%d)\n", 
	  IPC_msgInstanceName(msg), (int)qhndKey);
#endif
  void (*lispQueryHandler)(MSG_INSTANCE msg, void *data, int key);
  lispQueryHandler = lisp_call_address(_queryHandlerIndex);
  (*lispQueryHandler)(msg, data, (int)(size_t)qhndKey);
}

IPC_RETURN_TYPE subscribeHandlerChange (const char *msgName)
{
  return IPC_subscribeHandlerChange(msgName, ipcLispChangeHandler, NULL);
}

IPC_RETURN_TYPE unsubscribeHandlerChange (const char *msgName)
{
  return IPC_unsubscribeHandlerChange(msgName, ipcLispChangeHandler);
}

IPC_RETURN_TYPE queryResponse (const char *msgName, unsigned int length,
 		               BYTE_ARRAY content, IPC_VARCONTENT_PTR vc,
		               FORMAT_CONTAINER_TYPE *replyFormatContainer,
			       unsigned int timeoutMsecs)
{
  FORMATTER_PTR replyFormat;
  const char *replyMsgName = NULL;
  int ret = _IPC_queryResponse(msgName, length, content, &(vc->content),
			       &replyFormat, &replyMsgName, timeoutMsecs);
  if (replyFormatContainer != NULL) {
    replyFormatContainer->format = replyFormat;
    replyFormatContainer->msgName = (char *)replyMsgName;
  }
  return ret;
}

IPC_RETURN_TYPE queryNotify (const char *msgName, unsigned int length,
 		             BYTE_ARRAY content, unsigned int handlerIndex)
{
  return IPC_queryNotify(msgName, length, content, ipcLispQueryHandler,
                         (void *)(size_t)handlerIndex);
}

IPC_RETURN_TYPE printData (FORMATTER_PTR formatter, const char *fileName, 
                           IPC_VARCONTENT_PTR vc)
{
  FILE *file = fopen(fileName, "w");
  IPC_RETURN_TYPE retval;
  if (file == NULL) {
    retval = IPC_Error;
  } else {
    void *dataptr;
    retval = IPC_unmarshall(formatter, vc->content, &dataptr);
    if (retval == IPC_OK) {
      retval = IPC_printData(formatter, file, dataptr);
      IPC_freeData(formatter, dataptr);
    }
    fclose(file);
  }
  IPC_freeByteArray(vc->content);
  return retval;
}

static void ipcLispTimerHandler (void *hndIndex, unsigned long currentTime, 
				 unsigned long scheduledTime)
{
#if 0
  fprintf(stderr, "Calling ipcLispTimerHandler at %ld\n", currentTime);
#endif
  void (*lispTimerHandler)(void *data, unsigned long currentTime,
			   unsigned long scheduledTime);
  lispTimerHandler = lisp_call_address(_timerHandlerIndex);
  (*lispTimerHandler)(hndIndex, currentTime, scheduledTime);
}

IPC_RETURN_TYPE addTimerGetRef(unsigned long tdelay, long count,
			       unsigned long handlerIndex,
			       TIMER_REF_CONTAINER_TYPE *timerRefContainer)
{
  TIMER_REF timerRef;
  int ret = IPC_addTimerGetRef(tdelay, count, ipcLispTimerHandler,
			       (void *)handlerIndex, &timerRef);
  timerRefContainer->timerRef = timerRef;
  return ret;
}

// Should be rewritten to throw exception
static void ipcErrorProc (void)
{
  IPC_perror("IPC error detected");
}

void x_ipcRegisterExitProc(void (*proc)(void));

void setExitProc (void)
{
  x_ipcRegisterExitProc(ipcErrorProc);
}

