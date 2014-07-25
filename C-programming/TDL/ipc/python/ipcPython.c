/******************************************************************************
 * PROJECT: IPC (Interprocess Communication) Package
 *
 * (c) Copyright 2011 Reid Simmons.  All rights reserved.
 *
 * FILE: ipcPython.c
 *
 * ABSTRACT: C-code for interfacing specifically with Python
 *           Used by SWIG (see ffi/IPC.i)
 *
 *       $Id: ipcPython.c,v 1.3 2013/07/23 21:12:24 reids Exp $
 * $Revision: 1.3 $
 *     $Date: 2013/07/23 21:12:24 $
 *   $Author: reids $
 *    $State: Exp $
 *   $Locker:  $
 *
 * Copyright (c) 2011, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * REVISION HISTORY
 * $Log: ipcPython.c,v $
 * Revision 1.3  2013/07/23 21:12:24  reids
 * Minor editing and bug fix
 *
 * Revision 1.2  2012/02/27 16:55:46  reids
 * Fixed some problems with python and significantly improved transfer of arrays to/from python
 *
 * Revision 1.1  2011/08/16 16:00:36  reids
 * Adding Python interface to IPC
 *
 ****************************************************************/	

#include "ipcFFI.h"
#include "ipcPython.h"

extern const char *ipcErrorStrings[];

static void ipcPythonMsgHandler (MSG_INSTANCE msg, void *data, void *key)
{
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *hnd = PyObject_GetAttrString(IPC_module, "msgCallbackHandler");
  PyObject *pymsg = SWIG_NewPointerObj(msg, SWIGTYPE_p__X_IPC_REF, 0);
  PyObject *pydata = SWIG_NewPointerObj(data, SWIGTYPE_p_void, 0);
  PyObject *arglist = Py_BuildValue("(OOl)", pymsg, pydata, (long)key);
  PyEval_CallObject(hnd, arglist);
}

IPC_RETURN_TYPE subscribe (const char *msgName, const char *hndName, long key)
{
  return _IPC_subscribe(msgName, hndName, ipcPythonMsgHandler,
                        (void *)(size_t)key, 0);
}

static void ipcPythonFdHandler (int fd, void *key)
{
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *hnd = PyObject_GetAttrString(IPC_module, "fdCallbackHandler");
  PyObject *arglist = Py_BuildValue("(il)", fd, (long)key);
  PyEval_CallObject(hnd, arglist);
}

IPC_RETURN_TYPE subscribeFD (int fd, long key)
{
  return IPC_subscribeFD(fd, ipcPythonFdHandler, (void *)(size_t)key);
}

IPC_RETURN_TYPE unsubscribeFD (int fd)
{
  return IPC_unsubscribeFD(fd, ipcPythonFdHandler);
}

static void ipcPythonConnectHandler (const char *moduleName, void *clientData)
{
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *hnd = PyObject_GetAttrString(IPC_module, "connectCallbackHandler");
  PyObject *arglist = Py_BuildValue("(s)", moduleName);
  PyEval_CallObject(hnd, arglist);
}

static void ipcPythonDisconnectHandler (const char *moduleName,
                                        void *clientData)
{
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *hnd = PyObject_GetAttrString(IPC_module,
                                         "disconnectCallbackHandler");
  PyObject *arglist = Py_BuildValue("(s)", moduleName);
  PyEval_CallObject(hnd, arglist);
}

IPC_RETURN_TYPE subscribeConnect (void)
{
  return IPC_subscribeConnect(ipcPythonConnectHandler, NULL);
}

IPC_RETURN_TYPE subscribeDisconnect (void)
{
  return IPC_subscribeDisconnect(ipcPythonDisconnectHandler, NULL);
}

IPC_RETURN_TYPE unsubscribeConnect (void)
{
  return IPC_unsubscribeConnect(ipcPythonConnectHandler);
}

IPC_RETURN_TYPE unsubscribeDisconnect (void)
{
  return IPC_unsubscribeDisconnect(ipcPythonDisconnectHandler);
}

static void ipcPythonChangeHandler (const char *msgName, int numHandlers,
                                    void *clientData)
{
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *hnd = PyObject_GetAttrString(IPC_module, "changeCallbackHandler");
  PyObject *arglist = Py_BuildValue("(si)", msgName, numHandlers);
  PyEval_CallObject(hnd, arglist);
}

static void ipcPythonQueryHandler (MSG_INSTANCE msg, void *data, void *qhndKey)
{
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *hnd = PyObject_GetAttrString(IPC_module, "queryCallbackHandler");
  PyObject *pymsg = SWIG_NewPointerObj(msg, SWIGTYPE_p__X_IPC_REF, 0);
  PyObject *pydata = SWIG_NewPointerObj(data, SWIGTYPE_p_void, 0);
  PyObject *arglist = Py_BuildValue("(OOl)", pymsg, pydata, (long)qhndKey);
  PyEval_CallObject(hnd, arglist);
}

IPC_RETURN_TYPE subscribeHandlerChange (const char *msgName)
{
  return IPC_subscribeHandlerChange(msgName, ipcPythonChangeHandler, NULL);
}

IPC_RETURN_TYPE unsubscribeHandlerChange (const char *msgName)
{
  return IPC_unsubscribeHandlerChange(msgName, ipcPythonChangeHandler);
}

IPC_RETURN_TYPE queryResponse (const char *msgName, unsigned int length,
 		               BYTE_ARRAY content, IPC_VARCONTENT_PTR vc,
		               FORMATTER_CONTAINER_TYPE *replyFormatContainer,
			       unsigned int timeoutMsecs)
{
  FORMATTER_PTR replyFormat;
  const char *replyMsgName = NULL;
  int ret = _IPC_queryResponse(msgName, length, content, &(vc->content),
			       &replyFormat, &replyMsgName, timeoutMsecs);
  if (replyFormatContainer != NULL) {
    replyFormatContainer->formatter = replyFormat;
    replyFormatContainer->msgName = (char *)replyMsgName;
  }
  return ret;
}

IPC_RETURN_TYPE queryNotify (const char *msgName, unsigned int length,
 		             BYTE_ARRAY content, unsigned int handlerIndex)
{
  return IPC_queryNotify(msgName, length, content, ipcPythonQueryHandler,
                         (void *)(size_t)handlerIndex);
}

IPC_RETURN_TYPE printData (FORMATTER_PTR formatter, int fd, 
                           IPC_VARCONTENT_PTR vc)
{
  FILE *file = fdopen(fd, "w");
  void *dataptr;
  IPC_unmarshall(formatter, vc->content, &dataptr);
  IPC_RETURN_TYPE retval = IPC_printData(formatter, file, dataptr);
  IPC_freeData(formatter, dataptr);
  IPC_freeByteArray(vc->content);
  return retval;
}

static void ipcPythonTimerHandler (void *hndIndex, unsigned long currentTime, 
				   unsigned long scheduledTime)
{
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *hnd = PyObject_GetAttrString(IPC_module, "timerCallbackHandler");
  PyObject *arglist = Py_BuildValue("(lll)", (long)hndIndex, 
                                    currentTime, scheduledTime);
  PyEval_CallObject(hnd, arglist);
}

IPC_RETURN_TYPE addTimerGetRef(unsigned long tdelay, long count,
			       unsigned long handlerIndex,
			       TIMER_REF_CONTAINER_TYPE *timerRef)
{
  return IPC_addTimerGetRef(tdelay, count, ipcPythonTimerHandler,
	                    (void *)handlerIndex, &timerRef->timerRef);
}

// For Python, do not exit interpreter on error - raise exception, instead
static void ipcErrorProc (void)
{
  IPC_perror("IPC error detected");
  // Get the specific IPCError exception defined within Python (see below)
  PyObject *IPC_module = PyImport_AddModule("IPC");
  PyObject *IPCError = PyObject_GetAttrString(IPC_module, "IPCError");
  PyErr_SetString(IPCError, ipcErrorStrings[IPC_errno]);
}

void x_ipcRegisterExitProc(void (*proc)(void));

void setExitProc (void)
{
  x_ipcRegisterExitProc(ipcErrorProc);
}

// *WAY* more efficient to encode/decode a whole array at once,
// rather than element by element
void encodeByteArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutByte(buffer, (int32)PyInt_AsLong(PySequence_GetItem(pyArray, i)));
  }
}

void decodeByteArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i,
		       PyInt_FromLong((long)formatGetByte(buffer)));
  }
}

void encodeUByteArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutUByte(buffer,
		   (int32)PyInt_AsLong(PySequence_GetItem(pyArray, i)));
  }
}

void decodeUByteArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i
		       , PyInt_FromLong((long)formatGetUByte(buffer)));
  }
}

void encodeShortArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutShort(buffer, (int32)PyInt_AsLong(PySequence_GetItem(pyArray, i)));
  }
}

void decodeShortArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i,
		       PyInt_FromLong((long)formatGetShort(buffer)));
  }
}

void encodeIntArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutInt(buffer, (int32)PyInt_AsLong(PySequence_GetItem(pyArray, i)));
  }
}

void decodeIntArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i, PyInt_FromLong((long)formatGetInt(buffer)));
  }
}

SWIGINTERN int SWIG_AsVal_char (PyObject * obj, char *val);
SWIGINTERNINLINE PyObject *SWIG_From_char  (char c);

void encodeCharArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  char c;
  for (i=0; i<len; i++) {
    SWIG_AsVal_char(PySequence_GetItem(pyArray, i), &c);
    formatPutChar(buffer, c);
  }
}

void decodeCharArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i, SWIG_From_char(formatGetChar(buffer)));
  }
}

void encodeFloatArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutFloat(buffer,
		   (float)PyFloat_AsDouble(PySequence_GetItem(pyArray, i)));
  }
}

void decodeFloatArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i,
		       PyFloat_FromDouble((double)formatGetFloat(buffer)));
  }
}

void encodeDoubleArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutDouble(buffer, PyFloat_AsDouble(PySequence_GetItem(pyArray, i)));
  }
}

void decodeDoubleArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i,
		       PyFloat_FromDouble(formatGetDouble(buffer)));
  }
}

void encodeBooleanArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutInt(buffer, (int32)PyInt_AsLong(PySequence_GetItem(pyArray, i)));
  }
}

void decodeBooleanArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  fprintf(stderr, "HERE\n");
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i,
		       (formatGetInt(buffer) == 0 ? Py_False : Py_True));
  }
}

void encodeUShortArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutUShort(buffer, (int32)PyInt_AsLong(PySequence_GetItem(pyArray, i)));
  }
}

void decodeUShortArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i,
		       PyInt_FromLong((long)formatGetUShort(buffer)));
  }
}

void encodeUIntArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    formatPutUInt(buffer, (int32)PyInt_AsLong(PySequence_GetItem(pyArray, i)));
  }
}

void decodeUIntArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i,
		       PyInt_FromLong((long)formatGetUInt(buffer)));
  }
}

void encodeLongArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    long l = (long)PyInt_AsLong(PySequence_GetItem(pyArray, i));
    formatPutLong(buffer, l);
  }
}

void decodeLongArray (PyObject *pyArray, int32 len, BUFFER_PTR buffer)
{
  int i;
  for (i=0; i<len; i++) {
    PySequence_SetItem(pyArray, i, PyInt_FromLong(formatGetLong(buffer)));
  }
}
