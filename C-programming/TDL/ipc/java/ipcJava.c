/*****************************************************************************
 *       $Id: ipcJava.c,v 1.2 2013/07/24 01:43:08 reids Exp $
 * $Revision: 1.2 $
 *     $Date: 2013/07/24 01:43:08 $
 *   $Author: reids $
 *    $State: Exp $
 *   $Locker:  $
 *
 * PROJECT:	NM-DS1
 *
 * FILE:		ipcJava.c
 *
 * DESCRIPTION: JNI functions for the JAVA version of IPC.
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * HISTORY: Based on version by Carroll Thronesbery, Metrica (Sept 2000)
 *
 * $Log: ipcJava.c,v $
 * Revision 1.2  2013/07/24 01:43:08  reids
 * A few bugs fixed for the 3.10.0 version
 *
 * Revision 1.1  2013/07/23 21:11:38  reids
 * Updated for using SWIG
 *
 * Revision 1.7  2011/08/17 01:10:27  reids
 * Now uses ipcFFI
 *
 * Revision 1.6  2011/08/16 16:04:05  reids
 * Updated Java version to support 64-bit machines
 *
 * Revision 1.5  2009/09/04 19:11:20  reids
 * IPC Java is now in its own package
 *
 * Revision 1.4  2009/02/07 18:54:45  reids
 * Updates for use on 64 bit machines
 *
 * Revision 1.3  2009/01/12 15:54:55  reids
 * Added BSD Open Source license info
 *
 * Revision 1.2  2002/01/02 21:09:03  reids
 * Added another debugging function (printByteArray).
 *
 * Revision 1.1  2002/01/02 17:40:17  reids
 * Initial (and more, or less, complete) release of Java version of IPC.
 *
 *
 *****************************************************************************/

#include <stdio.h>
#include <jni.h>
#include "globalM.h"
#include "formatters.h"
#include "primFmttrs.h"
#include "ipcJava.h"

//#define JAVA

#define NOT_YET_SET 0xFFFFFFFF

#define MSG_CALLBACK_SIGNATURE     "(IJJ)V"
#define TIMER_CALLBACK_SIGNATURE   "(IJJ)V"
#define FD_CALLBACK_SIGNATURE      "(I)V"
#define CONNECT_CALLBACK_SIGNATURE "(Ljava/lang/String;Z)V"
#define CHANGE_CALLBACK_SIGNATURE  "(Ljava/lang/String;I)V"

JavaVM* pJavaVM = NULL;
jclass ipcClass                = (jclass)NOT_YET_SET;
jmethodID msgHandlerID         = (jmethodID)NOT_YET_SET;
jmethodID queryNotifyHandlerID = (jmethodID)NOT_YET_SET;
jmethodID timerHandlerID       = (jmethodID)NOT_YET_SET;
jmethodID fdHandlerID          = (jmethodID)NOT_YET_SET;
jmethodID connectHandlerID     = (jmethodID)NOT_YET_SET;
jmethodID disconnectHandlerID  = (jmethodID)NOT_YET_SET;
jmethodID changeHandlerID      = (jmethodID)NOT_YET_SET;

static void ipcJavaMsgHandler (MSG_INSTANCE msgInstance, BYTE_ARRAY callData,
			       void *handlerNum)
{
  JNIEnv* env;

  (*pJavaVM)->AttachCurrentThread(pJavaVM, (void **)(void *)&env, NULL);

  (*env)->CallStaticVoidMethod(env, ipcClass, msgHandlerID, 
			       (jint)(size_t)handlerNum,
			       (jlong)(size_t)msgInstance,
			       (jlong)(size_t)callData);
  (*pJavaVM)->DetachCurrentThread(pJavaVM);
}

static void ipcJavaQueryNotifyHandler (MSG_INSTANCE msgInstance, 
				       BYTE_ARRAY callData, void *handlerNum)
{
  JNIEnv* env;

  (*pJavaVM)->AttachCurrentThread(pJavaVM, (void **)(void *)&env, NULL);
  (*env)->CallStaticVoidMethod(env, ipcClass, queryNotifyHandlerID, 
			       (jint)(size_t)handlerNum,
			       (jlong)(size_t)msgInstance, 
			       (jlong)(size_t)callData);
  (*pJavaVM)->DetachCurrentThread(pJavaVM);
}

static void ipcJavaTimerHandler (void *handlerNum, unsigned long currentTime, 
				 unsigned long scheduledTime)
{
  JNIEnv* env;

  (*pJavaVM)->AttachCurrentThread(pJavaVM, (void **)(void *)&env, NULL);
  (*env)->CallStaticVoidMethod(env, ipcClass, timerHandlerID,
			       (jint)(size_t)handlerNum,
			       (jlong)(size_t)currentTime,
			       (jlong)(size_t)scheduledTime);
  (*pJavaVM)->DetachCurrentThread(pJavaVM);
}

static void ipcJavaFdHandler (int fd, void *handlerNum)
{
  JNIEnv* env;

  (*pJavaVM)->AttachCurrentThread(pJavaVM, (void **)(void *)&env, NULL);
  (*env)->CallStaticVoidMethod(env, ipcClass, fdHandlerID, (jint)fd);
  (*pJavaVM)->DetachCurrentThread(pJavaVM);
}

static void ipcJavaConnectHandler (const char *moduleName, void *clientData)
{
  JNIEnv* env;

  (*pJavaVM)->AttachCurrentThread(pJavaVM, (void **)(void *)&env, NULL);
  (*env)->CallStaticVoidMethod(env, ipcClass, connectHandlerID,
			       (*env)->NewStringUTF(env, moduleName),
			       ((BOOLEAN)(size_t)clientData ? JNI_TRUE : JNI_FALSE));
  (*pJavaVM)->DetachCurrentThread(pJavaVM);
}

static void ipcJavaChangeHandler (const char *msgName, int numHandlers,
				  void *clientData)
{
  JNIEnv* env;

  (*pJavaVM)->AttachCurrentThread(pJavaVM, (void **)(void *)&env, NULL);
  (*env)->CallStaticVoidMethod(env, ipcClass, changeHandlerID,
			       (*env)->NewStringUTF(env, msgName),
			       (jint)numHandlers);
  (*pJavaVM)->DetachCurrentThread(pJavaVM);
}

JNIEXPORT void JNICALL Java_ipc_java_ipcJava_registerIndices (JNIEnv *, jclass);
SWIGEXPORT void JNICALL Java_ipc_java_ipcJava_registerIndices (JNIEnv *jenv,
							       jclass jcls)
{
  (*jenv)->GetJavaVM(jenv, &pJavaVM);
  msgHandlerID = (*jenv)->GetStaticMethodID(jenv, jcls, "msgCallbackHandler",
					    MSG_CALLBACK_SIGNATURE);
  fdHandlerID = (*jenv)->GetStaticMethodID(jenv, jcls, "fdCallbackHandler",
					   FD_CALLBACK_SIGNATURE);
  connectHandlerID = (*jenv)->GetStaticMethodID(jenv, jcls,
						"connectCallbackHandler",
						CONNECT_CALLBACK_SIGNATURE);
  changeHandlerID = (*jenv)->GetStaticMethodID(jenv, jcls,
					       "changeCallbackHandler",
					       CHANGE_CALLBACK_SIGNATURE);
  queryNotifyHandlerID =
    (*jenv)->GetStaticMethodID(jenv, jcls, "queryNotifyCallbackHandler",
			       MSG_CALLBACK_SIGNATURE);
  timerHandlerID = (*jenv)->GetStaticMethodID(jenv, jcls,
					      "timerCallbackHandler",
					      TIMER_CALLBACK_SIGNATURE);
}

IPC_RETURN_TYPE subscribe (const char *msgName, const char *hndName, long key)
{
  return _IPC_subscribe(msgName, hndName, ipcJavaMsgHandler,
                        (void *)(size_t)key, 0);
}

IPC_RETURN_TYPE subscribeFD (int fd)
{
  return IPC_subscribeFD(fd, ipcJavaFdHandler, NULL);
}

IPC_RETURN_TYPE unsubscribeFD (int fd)
{
  return IPC_unsubscribeFD(fd, ipcJavaFdHandler);
}

IPC_RETURN_TYPE subscribeConnect (void)
{
  return IPC_subscribeConnect(ipcJavaConnectHandler, (void *)TRUE);
}

IPC_RETURN_TYPE subscribeDisconnect (void)
{
  return IPC_subscribeDisconnect(ipcJavaConnectHandler, (void *)FALSE);
}

IPC_RETURN_TYPE unsubscribeConnect ()
{
  return IPC_unsubscribeConnect(ipcJavaConnectHandler);
}

IPC_RETURN_TYPE unsubscribeDisconnect ()
{
  return IPC_unsubscribeDisconnect(ipcJavaConnectHandler);
}

IPC_RETURN_TYPE subscribeHandlerChange (const char *msgName)
{
  return IPC_subscribeHandlerChange(msgName, ipcJavaChangeHandler, NULL);
}

IPC_RETURN_TYPE unsubscribeHandlerChange (const char *msgName)
{
  return IPC_unsubscribeHandlerChange(msgName, ipcJavaChangeHandler);
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

IPC_RETURN_TYPE queryNotify (const char *msgName, unsigned int length,
			     BYTE_ARRAY content, unsigned int handlerIndex)
{
  return  IPC_queryNotify(msgName, (int)length, content,
			  ipcJavaQueryNotifyHandler,
			  (void *)(size_t)handlerIndex);
}

IPC_RETURN_TYPE addTimerGetRef(unsigned long tdelay, long count,
			       unsigned long handlerIndex,
			       TIMER_REF_CONTAINER_TYPE *timerRefContainer)
{
  TIMER_REF timerRef;
  int ret = IPC_addTimerGetRef(tdelay, count, ipcJavaTimerHandler,
			       (void *)handlerIndex, &timerRef);
  if (timerRefContainer != NULL) timerRefContainer->timerRef = timerRef;
  return ret;
}
