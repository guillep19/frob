/******************************************************************************
 * PROJECT: IPC (Interprocess Communication) Package
 *
 * (c) Copyright 2011 Reid Simmons.  All rights reserved.
 *
 * FILE: ipcLisp.h
 *
 * ABSTRACT: C-headers for interfacing specifically with Lisp
 *           Used by SWIG (see ffi/IPC.i)
 *
 *       $Id: ipcLisp.h,v 2.1 2013/07/23 21:18:25 reids Exp $
 * $Revision: 2.1 $
 *     $Date: 2013/07/23 21:18:25 $
 *   $Author: reids $
 *    $State: Exp $
 *   $Locker:  $
 *
 * Copyright (c) 2011, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * REVISION HISTORY
 * $Log: ipcLisp.h,v $
 * Revision 2.1  2013/07/23 21:18:25  reids
 * Updated for using SWIG
 *
 *
 ****************************************************************/	

#ifndef IPC_LISP_H
#define IPC_LISP_H

void registerIndices (int msgHandlerIndex, int queryHandlerIndex,
		      int fdHandlerIndex, int connectHandlerIndex,
		      int disconnectHandlerIndex, int changeHandlerIndex,
		      int timerHandlerIndex);

// Needed for queryResponse, to pass back replyFormat
typedef struct {
  FORMATTER_PTR format;
  char *msgName; } FORMAT_CONTAINER_TYPE;
// Needed for addTimerGetRef, to pass back timerRef
typedef struct { TIMER_REF timerRef; } TIMER_REF_CONTAINER_TYPE;

IPC_RETURN_TYPE _IPC_unsubscribe (const char *msgName, const char *hndName);
IPC_RETURN_TYPE subscribe (const char *msgName, const char *hndName, long key);
IPC_RETURN_TYPE subscribeFD (int fd, long key);
IPC_RETURN_TYPE unsubscribeFD (int fd);
IPC_RETURN_TYPE subscribeConnect (void);
IPC_RETURN_TYPE subscribeDisconnect (void);
IPC_RETURN_TYPE unsubscribeConnect (void);
IPC_RETURN_TYPE unsubscribeDisconnect (void);
IPC_RETURN_TYPE subscribeHandlerChange (const char *msgName);
IPC_RETURN_TYPE unsubscribeHandlerChange (const char *msgName);
IPC_RETURN_TYPE queryResponse (const char *msgName, unsigned int length,
 		               BYTE_ARRAY content, IPC_VARCONTENT_TYPE *vc,
		               FORMAT_CONTAINER_TYPE *replyFormat,
			       unsigned int timeoutMsecs);
IPC_RETURN_TYPE queryNotify (const char *msgName, unsigned int length,
 		               BYTE_ARRAY content, unsigned int handlerIndex);
IPC_RETURN_TYPE printData (FORMATTER_PTR formatter, const char *fileName,
                           IPC_VARCONTENT_TYPE *vc);
IPC_RETURN_TYPE addTimerGetRef(unsigned long tdelay, long count,
			       unsigned long handlerIndex,
			       TIMER_REF_CONTAINER_TYPE *timerRefContainer);

void setExitProc (void);

#endif // IPC_LISP_H
