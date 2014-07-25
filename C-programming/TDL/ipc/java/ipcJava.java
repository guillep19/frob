/****************************************************************
 * PROJECT: IPC (Interprocess Communication) Package
 *
 * (c) Copyright 2013 Reid Simmons.  All rights reserved.
 *
 * FILE: ipcJava.java
 *
 * ABSTRACT: Java-code for interfacing specifically with the C version of IPC
 *           Used by SWIG (see IPC.i)
 *
 *       $Id: ipcJava.java,v 1.1 2013/07/23 21:11:38 reids Exp $
 * $Revision: 1.1 $
 *     $Date: 2013/07/23 21:11:38 $
 *   $Author: reids $
 *    $State: Exp $
 *   $Locker:  $
 *
 * Copyright (c) 2011, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * REVISION HISTORY
 * $Log: ipcJava.java,v $
 * Revision 1.1  2013/07/23 21:11:38  reids
 * Updated for using SWIG
 *
 *
 ****************************************************************/

package ipc.java;

import java.util.*;
import java.io.*;
import java.nio.file.*;

public class ipcJava {

  static { System.loadLibrary("ipcjava"); }
  public final static native void registerIndices();

  /*****************************************************************
   *
   *    CLASSES AND INTERFACES NEEDED TO INTERFACE WITH IPC
   *
   ****************************************************************/

  public interface HANDLER_TYPE {
    void handle (MSG_INSTANCE msgInstance, Object callData, Object clientData);
  }

  public interface FD_HANDLER_TYPE {
    void handle (int fd, Object clientData);
  }

  public interface CONNECT_HANDLER_TYPE {
    void handle (String moduleName, Object clientData);
  }

  public interface CHANGE_HANDLER_TYPE {
    void handle (String msgName, int numHandlers, Object clientData);
  }

  public interface TIMER_HANDLER_TYPE {
    void handle (Object clientData, long currentTime, long scheduledTime);
  }

  private static class handlerClass {
    Object clientData;
  }

  private static class msgHandlerData extends handlerClass {
    msgHandlerData (HANDLER_TYPE _handler, Object _clientData,
		    boolean _unmarshall) {
      handler = _handler; clientData = _clientData; unmarshall = _unmarshall;
    }
    HANDLER_TYPE handler;
    boolean unmarshall;
  }

  private static class fdHandlerData extends handlerClass {
    fdHandlerData (FD_HANDLER_TYPE _handler, Object _clientData){
      handler = _handler; clientData = _clientData;
    }
    FD_HANDLER_TYPE handler;
  }

  private static class connectHandlerData extends handlerClass {
    connectHandlerData (CONNECT_HANDLER_TYPE _handler, Object _clientData){
      handler = _handler; clientData = _clientData;
    }
    CONNECT_HANDLER_TYPE handler;
  }

  private static class changeHandlerData extends handlerClass {
    changeHandlerData (CHANGE_HANDLER_TYPE _handler, Object _clientData){
      handler = _handler; clientData = _clientData;
    }
    CHANGE_HANDLER_TYPE handler;
  }

  private static class timerHandlerData extends handlerClass {
    timerHandlerData (TIMER_HANDLER_TYPE _handler, SWIGTYPE_p_void _timerRef,
		      Object _clientData){
      handler = _handler; timerRef = _timerRef; clientData = _clientData;
    }
    TIMER_HANDLER_TYPE handler;
    SWIGTYPE_p_void timerRef;
  }

  /*****************************************************************
   *
   *    IPC API FUNCTIONS FOR JAVA
   *
   ****************************************************************/
  
    private static Map msgHashTable =
      Collections.synchronizedMap(new HashMap());
    private static Map fdHashTable =
	Collections.synchronizedMap(new HashMap());
    private static Map timerHashTable =
	Collections.synchronizedMap(new HashMap());
    private static List connectHandlers =
	Collections.synchronizedList(new ArrayList());
    private static List disconnectHandlers = 
	Collections.synchronizedList(new ArrayList());
    private static Map handlerChangeHashTable =
	Collections.synchronizedMap(new HashMap());
    private static Map msgClassHashTable =
	Collections.synchronizedMap(new HashMap());
    private static int handlerNumber = 0;

    private static boolean boolVal (int val) { return (val != 0); }

    private static void handleException(String where, String msgName, 
					Exception e) {
      System.err.println("ERROR: "+ where +": For message \""+msgName +"\": "+ 
			 e.getMessage());
      e.printStackTrace();
    }

    private static Class getMsgClass (String msgName,
				      FORMAT_TYPE formatter) throws Exception {
      if (formatter == null || FORMAT_TYPE.getCPtr(formatter) == 0) {
        return Object.class;
      } else {
        Class oclass = (Class)msgClassHashTable.get(msgName);
	if (oclass == null)
	  throw new Exception("Missing class associated with message "+
			      msgName);
	return oclass;
      }
    }

    private static void msgCallbackHandler (int handlerNum, long msgInstanceP,
					    long byteArrayP) {
      MSG_INSTANCE msgInstance = new MSG_INSTANCE(msgInstanceP, true);
      SWIGTYPE_p_void byteArray = new SWIGTYPE_p_void(byteArrayP, true);
      msgHandlerData handlerData = 
	(msgHandlerData)msgHashTable.get(Integer.toString(handlerNum));
      if (handlerData == null) {
	System.out.println("Ooops -- no handler for "+
			   IPC.IPC_msgInstanceName(msgInstance));
      } else if (handlerData.unmarshall) {
	try {
	  FORMAT_TYPE formatter = IPC.IPC_msgInstanceFormatter(msgInstance);
	  Class oclass = getMsgClass(IPC.IPC_msgInstanceName(msgInstance),
				     formatter);
	  Object object = IPC.IPC_unmarshallData(formatter, byteArray, oclass);
	  handlerData.handler.handle(msgInstance, object,
				     handlerData.clientData);
	} catch ( Exception e) {
	  handleException("msgCallbackHandler", 
			  IPC.IPC_msgInstanceName(msgInstance), e);
	}
	IPC.IPC_freeByteArray(byteArray);
      } else {
	handlerData.handler.handle(msgInstance, byteArray,
				   handlerData.clientData);
      }
    }

    private static void fdCallbackHandler (int fd) {
      //System.out.println("fdCallbackHandler");
      fdHandlerData hndData =
	  (fdHandlerData)fdHashTable.get(Integer.toString(fd));
      if (hndData == null) {
	System.out.println("Ooops -- no handler for file descriptor "+fd);
      } else {
	hndData.handler.handle(fd, hndData.clientData);
      }
    }

    private static void connectCallbackHandler (String moduleName,
						boolean isConnect) {
      //System.out.println("connectCallbackHandler "+isConnect);
      Iterator iter = (isConnect ? connectHandlers.iterator()
		       : disconnectHandlers.iterator());

      while (iter.hasNext()) {
	connectHandlerData hndData = (connectHandlerData)iter.next();
	hndData.handler.handle(moduleName, hndData.clientData);
      }
    }

    private static void changeCallbackHandler (String msgName,
					       int numHandlers) {
      //System.out.println("changeCallbackHandler");
      List handlerList = (List)handlerChangeHashTable.get(msgName);
      if (handlerList == null) {
	System.out.println("Ooops -- no change handlers for message "+msgName);
      } else {
	Iterator iter = handlerList.iterator();

	while (iter.hasNext()) {
	  changeHandlerData hndData = (changeHandlerData)iter.next();
	  hndData.handler.handle(msgName, numHandlers, hndData.clientData);
	}
      }
    }

    private static void queryNotifyCallbackHandler (int handlerNum,
						    long msgInstanceP,
						    long byteArrayP) {
      //System.out.println("queryNotifyCallbackHandler");
      MSG_INSTANCE msgInstance = new MSG_INSTANCE(msgInstanceP, true);
      SWIGTYPE_p_void byteArray = new SWIGTYPE_p_void(byteArrayP, true);
      String hashKey = Integer.toString(handlerNum);
      msgHandlerData handlerData = (msgHandlerData)msgHashTable.get(hashKey);
      msgHashTable.remove(hashKey);
      if (handlerData == null) {
	System.out.println("Ooops -- no query notification handler for "+
			   IPC.IPC_msgInstanceName(msgInstance));
      } else if (handlerData.unmarshall) {
	try {
	  FORMAT_TYPE formatter = IPC.IPC_msgInstanceFormatter(msgInstance);
	  Class oclass = getMsgClass(IPC.IPC_msgInstanceName(msgInstance),
				     formatter);
	  Object object = IPC.IPC_unmarshallData(formatter, byteArray, oclass);
	  handlerData.handler.handle(msgInstance, object,
				     handlerData.clientData);
	} catch ( Exception e) {
	  handleException("queryNotifyCallbackHandler", 
			  IPC.IPC_msgInstanceName(msgInstance), e);
	}
	IPC.IPC_freeByteArray(byteArray);
      } else {
	handlerData.handler.handle(msgInstance, byteArray,
				   handlerData.clientData);
      }

    }

    private static void timerCallbackHandler (int handlerNum, long currentTime,
					      long scheduledTime) {
      //System.out.println("timerCallbackHandler");
      String hashKey = Integer.toString(handlerNum);
      timerHandlerData hndData = (timerHandlerData)timerHashTable.get(hashKey);

      if (hndData != null) {
	hndData.handler.handle(hndData.clientData, currentTime, scheduledTime);
	if (IPC.maxTriggers(hndData.timerRef) == 0) {
	  System.out.println("Deleting timer "+
			     hndData.handler.getClass().getName());
	  timerHashTable.remove(hndData.handler.getClass().getName());
	  timerHashTable.remove(hashKey);
	}
    } else
      System.out.println("Ooops -- no handler for timer");
    }

    private static String msgHandlerName (String msgName, Class handlerClass) {
      return msgName +"_"+ handlerClass.getName();
    }

    public static void initJavaIPC() {
      registerIndices();
      msgHashTable.clear();
      fdHashTable.clear();
      timerHashTable.clear();
      connectHandlers.clear();
      disconnectHandlers.clear();
      handlerChangeHashTable.clear();
      msgClassHashTable.clear();
      handlerNumber = 0;
    }

    public static IPC_RETURN_TYPE IPC_initialize () {
      initJavaIPC();
      return IPC._IPC_initialize();
    }

    public static IPC_RETURN_TYPE IPC_connect (String module) {
      initJavaIPC();
      return IPC._IPC_connect(module);
    }

    public static IPC_RETURN_TYPE IPC_connectModule (String module,
						     String server) {
      initJavaIPC();
      return IPC._IPC_connectModule(module, server);
    }

    public static IPC_RETURN_TYPE IPC_connectNoListen (String module) {
      initJavaIPC();
      return IPC._IPC_connectNoListen(module);
    }

    public static IPC_RETURN_TYPE IPC_connectModuleNoListen (String module,
							     String server) {
      initJavaIPC();
      return IPC._IPC_connectModuleNoListen(module, server);
    }

    public static boolean IPC_isConnected () {
      return boolVal(IPC._IPC_isConnected());
    }

    public static boolean IPC_isModuleConnected (String module) {
      return boolVal(IPC._IPC_isModuleConnected(module));
    }

    public static boolean IPC_isMsgDefined (String msgName) {
      return boolVal(IPC._IPC_isMsgDefined(msgName));
    }

    public static IPC_RETURN_TYPE 
	IPC_marshall (FORMAT_TYPE formatter, Object object, 
		      IPC_VARCONTENT_TYPE varcontent) throws Exception {
	return formatters.marshall(formatter, coerceDataObject(object),
				   varcontent);
    }

    public static IPC_RETURN_TYPE
	IPC_unmarshall (FORMAT_TYPE formatter, SWIGTYPE_p_void byteArray,
			Object object) throws Exception {
	Class dclass = object.getClass();
	if (coerceDataClass(dclass) != dclass) {
	  System.out.println("ERROR: "+object+
			     " not assignable in IPC_unmarshall");
	  return IPC_RETURN_TYPE.IPC_Error;
	} else {
	  return formatters.unmarshall(formatter, byteArray, object);
	}
    }

    public static Object IPC_unmarshallData (FORMAT_TYPE formatter,
					     SWIGTYPE_p_void byteArray,
					     Class theClass) throws Exception {
      // No data or formatter
      if (byteArray == null || formatter == null) return null;
      else {
	// Throw an exception if class does not match formatter
	formatters.checkDataClass(formatter, theClass);

	// Create an object type that Java IPC can handle
	Object object = 
	  (theClass.isArray() ?
	   // Create a top-level fixed-length array, based on the formatter.
	   formatters.createFixedArray(theClass.getComponentType(), formatter):
	   coerceDataClass(theClass).newInstance());
	IPC_RETURN_TYPE ret = formatters.unmarshall(formatter,
						    byteArray, object);
	if (formatters.IPCPrim.class.isAssignableFrom(object.getClass()))
	    object = ((formatters.IPCPrim)object).coerce();
	return (ret == IPC_RETURN_TYPE.IPC_OK ? object : null);
      }
    }

  public static IPC_RETURN_TYPE IPC_publishData (String msgName, Object data) {
    IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
    IPC_RETURN_TYPE retVal = IPC_RETURN_TYPE.IPC_OK;
    if (data != null) {
      try { retVal = IPC.IPC_marshall(IPC.IPC_msgFormatter(msgName), 
				      coerceDataObject(data), vc);
      } catch ( Exception e) {
	handleException("IPC_publishData", msgName, e);
      }
    }
    if (retVal == IPC_RETURN_TYPE.IPC_OK) {
      retVal = IPC.IPC_publishVC(msgName, vc);
      if (vc.getLength() != 0) IPC.IPC_freeByteArray(vc.getContent());
    }
    return retVal;
  }

  private static IPC_RETURN_TYPE
    subscribe (String msgName, HANDLER_TYPE msgHandler, Object clientData,
	       boolean unmarshall) { 
      /* Do it this way because multiple handlers can be subscribed 
	 for the same message */
      String handlerName = msgHandlerName(msgName, msgHandler.getClass());
      String hashKey = (String)msgHashTable.get(handlerName);
      if (hashKey != null) {
	msgHandlerData hndData = (msgHandlerData)msgHashTable.get(hashKey);
	if (hndData.clientData != clientData) {
	  System.out.println("Resetting client data for handler "+handlerName);
	  hndData.clientData = clientData;
	}
	hndData.handler = msgHandler;
	hndData.unmarshall = unmarshall;
	return IPC_RETURN_TYPE.IPC_OK;
      } else {
	int key = ++handlerNumber;
	hashKey = Integer.toString(key);
	msgHashTable.put(handlerName, hashKey);
	msgHashTable.put(hashKey, new msgHandlerData(msgHandler, clientData,
						     unmarshall));
	int ret = IPCJNI.subscribe(msgName, handlerName, key);
	return (ret == 0 ? IPC_RETURN_TYPE.IPC_Error : IPC_RETURN_TYPE.IPC_OK);
      }
  }

  // Associate the class type with the message
  public static IPC_RETURN_TYPE IPC_msgClass (String msgName, Class oclass) { 
    msgClassHashTable.put(msgName, oclass);
    return IPC_RETURN_TYPE.IPC_OK;
  }

  public static IPC_RETURN_TYPE IPC_subscribeData (String msgName,
						   HANDLER_TYPE msgHandler) { 
    return IPC_subscribeData(msgName, msgHandler, null);
  }
  public static IPC_RETURN_TYPE IPC_subscribeData (String msgName,
						   HANDLER_TYPE msgHandler,
						   Object clientData) { 
    return subscribe(msgName, msgHandler, clientData, true);
  }

  public static IPC_RETURN_TYPE IPC_subscribe (String msgName,
					       HANDLER_TYPE msgHandler) { 
    return IPC_subscribe(msgName, msgHandler, null);
  }
  public static IPC_RETURN_TYPE IPC_subscribe (String msgName,
					       HANDLER_TYPE msgHandler,
					       Object clientData) { 
    return subscribe(msgName, msgHandler, clientData, false);
  }

  /* Do it this way because multiple handlers can be subscribed for the
     same message */
  public static IPC_RETURN_TYPE IPC_unsubscribe (String msgName,
						 Class msgHandlerClass) { 
    String handlerName = msgHandlerName(msgName, msgHandlerClass);
    String hashKey = (String)msgHashTable.get(handlerName);

    msgHashTable.remove(handlerName);
    msgHashTable.remove(hashKey);

    return IPC._IPC_unsubscribe(msgName, handlerName);
  }

  public static IPC_RETURN_TYPE IPC_unsubscribe (String msgName,
						 HANDLER_TYPE msgHandler) { 
    return IPC_unsubscribe(msgName, msgHandler.getClass());
  }

  public static IPC_RETURN_TYPE IPC_subscribeFD (int fd,
						 FD_HANDLER_TYPE fdHandler) {
    return IPC_subscribeFD (fd, fdHandler, null);
  }
  public static IPC_RETURN_TYPE
      IPC_subscribeFD (int fd, FD_HANDLER_TYPE fdHandler, Object clientData) {

    fdHandlerData hndData =
	(fdHandlerData)fdHashTable.get(Integer.toString(fd));
    if (hndData == null) {
      fdHashTable.put(Integer.toString(fd),
		      new fdHandlerData(fdHandler, clientData));
      return IPC.subscribeFD(fd);
    } else if (hndData.handler.getClass() != fdHandler.getClass()) {
      System.out.println("WARNING: Handler function replaced for fd "+fd);
      hndData.handler = fdHandler; hndData.clientData = clientData;
    } else if (hndData.clientData != clientData) {
      System.out.println("WARNING: Replacing client data for fd "+fd);
      hndData.clientData = clientData;
    }
    return IPC_RETURN_TYPE.IPC_OK;
  }

  public static IPC_RETURN_TYPE IPC_unsubscribeFD (int fd,
						   Class fdHandlerClass) {
    fdHandlerData hndData =
	(fdHandlerData)fdHashTable.get(Integer.toString(fd));
    if (hndData == null || hndData.handler.getClass() != fdHandlerClass) {
      System.out.println("Ooops -- handler "+fdHandlerClass.getName()+
			 " not subscribed for file descriptor "+fd);
      return IPC_RETURN_TYPE.IPC_Error;
    } else {
      fdHashTable.remove(Integer.toString(fd));
      return IPC.unsubscribeFD(fd);
    }
  }

  public static IPC_RETURN_TYPE IPC_unsubscribeFD (int fd,
						   FD_HANDLER_TYPE fdHandler) {
    return IPC_unsubscribeFD(fd, fdHandler.getClass());
  }

  private static connectHandlerData findConnectHandler (List handlers,
							Class conHandlerClass){
    Iterator iter = handlers.iterator();
    while (iter.hasNext()) {
      connectHandlerData data = (connectHandlerData)iter.next();
      if (data.handler.getClass() == conHandlerClass) return data;
    }
    return null;
  }

  public static IPC_RETURN_TYPE 
      IPC_subscribeConnect (CONNECT_HANDLER_TYPE conHandler) {
    return IPC_subscribeConnect (conHandler, null);
  }
  public static IPC_RETURN_TYPE
    IPC_subscribeConnect (CONNECT_HANDLER_TYPE conHandler, Object clientData) {
      connectHandlerData hndData = findConnectHandler(connectHandlers,
						      conHandler.getClass());
      if (hndData == null) {
	connectHandlers.add(0, new connectHandlerData(conHandler, clientData));
	return (connectHandlers.size() == 1 ?
		IPC.subscribeConnect() : IPC_RETURN_TYPE.IPC_OK);
      } else if (hndData.clientData != clientData) {
	System.out.println("WARNING: Replacing connect handler client data");
	hndData.clientData = clientData;
      }
      return IPC_RETURN_TYPE.IPC_OK;
  }

  public static IPC_RETURN_TYPE 
      IPC_subscribeDisconnect (CONNECT_HANDLER_TYPE disconHandler) {
    return IPC_subscribeDisconnect (disconHandler, null);
  }
  public static IPC_RETURN_TYPE
    IPC_subscribeDisconnect (CONNECT_HANDLER_TYPE disconHandler,
			     Object clientData) {
    connectHandlerData hndData = findConnectHandler(disconnectHandlers,
						    disconHandler.getClass());
    if (hndData == null) {
      disconnectHandlers.add(0, new connectHandlerData(disconHandler,
						       clientData));
      return (disconnectHandlers.size() == 1 ?
	      IPC.subscribeDisconnect() : IPC_RETURN_TYPE.IPC_OK);
    } else if (hndData.clientData != clientData) {
      System.out.println("WARNING: Replacing disconnect handler client data");
      hndData.clientData = clientData;
    }
    return IPC_RETURN_TYPE.IPC_OK;
  }

  public static IPC_RETURN_TYPE
      IPC_unsubscribeConnect (Class connectHandlerClass) {
    connectHandlerData hndData = findConnectHandler(connectHandlers,
						    connectHandlerClass);
    if (hndData == null) {
      System.err.println("No connection handler found for class "+
			 connectHandlerClass.getName());
      return IPC_RETURN_TYPE.IPC_Error;
    } else {
      connectHandlers.remove(hndData);
      return (connectHandlers.isEmpty()
	      ? IPC.unsubscribeConnect() : IPC_RETURN_TYPE.IPC_OK);
    }
  }

  public static IPC_RETURN_TYPE
      IPC_unsubscribeConnect (CONNECT_HANDLER_TYPE connectHandler) {
    return IPC_unsubscribeConnect(connectHandler.getClass());
  }

  public static IPC_RETURN_TYPE
      IPC_unsubscribeDisconnect (Class disconnectHandlerClass) {
    connectHandlerData hndData = findConnectHandler(disconnectHandlers,
						    disconnectHandlerClass);
    if (hndData == null) {
      System.out.println("No disconnection handler found for class "+
			 disconnectHandlerClass.getName());
      return IPC_RETURN_TYPE.IPC_Error;
    } else {
      disconnectHandlers.remove(hndData);
      return (disconnectHandlers.isEmpty()
	      ? IPC.unsubscribeDisconnect() : IPC_RETURN_TYPE.IPC_OK);
    }
  }

  public static IPC_RETURN_TYPE
      IPC_unsubscribeDisconnect (CONNECT_HANDLER_TYPE disconnectHandler) {
    return IPC_unsubscribeDisconnect(disconnectHandler.getClass());
  }

  private static changeHandlerData findChangeHandler (List handlers,
						      Class chgHandlerClass){
    Iterator iter = handlers.iterator();
    while (iter.hasNext()) {
      changeHandlerData data = (changeHandlerData)iter.next();
      if (data.handler.getClass() == chgHandlerClass) return data;
    }
    return null;
  }

  public static IPC_RETURN_TYPE
      IPC_subscribeHandlerChange (String msgName, 
				  CHANGE_HANDLER_TYPE handlerChangeHandler) {
    return IPC_subscribeHandlerChange (msgName, handlerChangeHandler, null);
  }
  public static IPC_RETURN_TYPE
      IPC_subscribeHandlerChange (String msgName, 
				  CHANGE_HANDLER_TYPE handlerChangeHandler,
				  Object clientData) {
    /* Do it this way because multiple handlers can be subscribed 
       for same message */
    List list = (List)handlerChangeHashTable.get(msgName);
    if (list == null || list.isEmpty()) {
      if (list == null) list = new LinkedList();
      handlerChangeHashTable.put(msgName, list);
      list.add(0, new changeHandlerData(handlerChangeHandler, clientData));
      return IPC.subscribeHandlerChange(msgName);
    } else {
      changeHandlerData hndData = 
	  findChangeHandler(list, handlerChangeHandler.getClass());
      if (hndData == null) {
	list.add(0, new changeHandlerData(handlerChangeHandler, clientData));
	return IPC_RETURN_TYPE.IPC_OK; 
      } else if (hndData.clientData != clientData) {
	System.out.println("WARNING: Replacing change handler client data for "
			   +hndData.handler.getClass().getName());
	hndData.clientData = clientData;
      }
      return IPC_RETURN_TYPE.IPC_OK;
    }
  }

  public static IPC_RETURN_TYPE
      IPC_unsubscribeHandlerChange (String msgName, Class changeHandlerClass) {
    List handlerList = (List)handlerChangeHashTable.get(msgName);
    if (handlerList == null || handlerList.isEmpty()) {
      System.err.println("No change handler found on "+msgName+" for class "+
			 changeHandlerClass.getName());
      return IPC_RETURN_TYPE.IPC_Error;
    } else {
      changeHandlerData hndData = findChangeHandler(handlerList,
						    changeHandlerClass);
      if (hndData == null) {
	System.err.println("No change handler found on "+msgName+" for class "+
			   changeHandlerClass.getName());
	return IPC_RETURN_TYPE.IPC_Error;
      } else {
	handlerList.remove(hndData);
	return (handlerList.isEmpty() ? IPC.unsubscribeHandlerChange(msgName) :
		IPC_RETURN_TYPE.IPC_OK);
      }
    }
  }

  public static IPC_RETURN_TYPE
      IPC_unsubscribeHandlerChange (String msgName,
				    CHANGE_HANDLER_TYPE changeHandler){
    return IPC_unsubscribeHandlerChange(msgName, changeHandler.getClass());
  }

  // This is pretty kludgy and inefficient, but it works!
  public static IPC_RETURN_TYPE
      IPC_printData (FORMAT_TYPE formatter, OutputStream ostream,
		     Object data) throws Exception {
    IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
    IPC_RETURN_TYPE retval = IPC_RETURN_TYPE.IPC_OK;
    retval = IPC.IPC_marshall(formatter, data, vc);
    if (retval != IPC_RETURN_TYPE.IPC_OK) return retval;
    else {
      retval = IPC.printData(formatter, ".pd.tmp", vc);
      if (retval != IPC_RETURN_TYPE.IPC_OK) return retval;
      else {
        Path path = Paths.get(".pd.tmp");
	Files.copy(path, ostream);
	Files.delete(path);
      }
      return retval;
    }
  }

  public static IPC_RETURN_TYPE
      IPC_readData (FORMAT_TYPE formatter, InputStream ostream, Object data){
    System.err.println("IPC_readData: Not yet implemented");
    return IPC_RETURN_TYPE.IPC_Error;
  }

  public static IPC_RETURN_TYPE
      IPC_respondData (MSG_INSTANCE msgInstance, String msgName, Object data) {
    IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
    IPC_RETURN_TYPE retVal = IPC_RETURN_TYPE.IPC_OK;
    FORMAT_TYPE formatter = IPC.IPC_msgFormatter(msgName);
    
    if (data != null) {
      try { retVal = IPC.IPC_marshall(formatter, coerceDataObject(data), vc);
      } catch ( Exception e) {
	handleException("IPC_respondData", msgName, e);
      }
    }
    if (retVal == IPC_RETURN_TYPE.IPC_OK) {
      retVal = IPC.IPC_respondVC(msgInstance, msgName, vc);
      if (vc.getLength() != 0) IPC.IPC_freeByteArray(vc.getContent());
    }
    return retVal;
  }

  public static SWIGTYPE_p_void
      IPC_queryResponse (String msgName, int length, SWIGTYPE_p_void content,
			 long timeout) {
    IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
    IPC_RETURN_TYPE ret = IPC.queryResponse(msgName, length, content, vc,
					    null, timeout);
    return (ret == IPC_RETURN_TYPE.IPC_OK ? vc.getContent() : null);
  }

  public static SWIGTYPE_p_void
      IPC_queryResponseVC (String msgName, IPC_VARCONTENT_TYPE varcontent,
			   long timeout) {
    return IPC.IPC_queryResponse(msgName, (int)varcontent.getLength(),
				 varcontent.getContent(), timeout);
  }

  public static Object 
      IPC_queryResponseData (String msgName, Object data,
			     long timeoutMSecs) throws Exception {
    Object responseObject = null;
    IPC_VARCONTENT_TYPE varcontent = new IPC_VARCONTENT_TYPE();
    IPC_RETURN_TYPE ret = IPC.IPC_marshall(IPC.IPC_msgFormatter(msgName), 
					   data, varcontent);
    if (ret != IPC_RETURN_TYPE.IPC_OK) {
      return ret;
    } else {
      IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
      FORMATTER_CONTAINER_TYPE replyFormat = new FORMATTER_CONTAINER_TYPE();
      ret = IPC.queryResponse(msgName, varcontent.getLength(),
			      varcontent.getContent(), vc, replyFormat,
			      timeoutMSecs);
      if (varcontent.getLength() != 0)
	IPC.IPC_freeByteArray(varcontent.getContent());
      if (ret == IPC_RETURN_TYPE.IPC_OK) {
	Class responseClass = getMsgClass(replyFormat.getMsgName(),
					  replyFormat.getFormatter());
	return IPC.IPC_unmarshallData(replyFormat.getFormatter(),
				      vc.getContent(), responseClass);
      } else return null;
    }
  }

  public static IPC_RETURN_TYPE
      _IPC_queryNotify (String msgName, long length, SWIGTYPE_p_void byteArray,
			HANDLER_TYPE handler, Object clientData,
			boolean unmarshall) {
      int key = ++handlerNumber;
      msgHashTable.put(Integer.toString(key), 
		       new msgHandlerData(handler, clientData, unmarshall));
      return IPC.queryNotify(msgName, length, byteArray, key);
  }

  public static IPC_RETURN_TYPE IPC_queryNotify (String msgName, int length,
						 SWIGTYPE_p_void byteArray,
						 HANDLER_TYPE handler) {
    return IPC_queryNotify (msgName, length, byteArray, handler, null);
  }

  public static IPC_RETURN_TYPE
      IPC_queryNotify (String msgName, int length, SWIGTYPE_p_void byteArray,
		       HANDLER_TYPE handler, Object clientData) {
    return _IPC_queryNotify(msgName, length, byteArray,
			    handler,clientData, false);
  }

  public static IPC_RETURN_TYPE IPC_queryNotifyVC (String msgName,
						   IPC_VARCONTENT_TYPE vc,
						   HANDLER_TYPE handler) {
    return IPC_queryNotifyVC (msgName, vc, handler, null);
  }
  public static IPC_RETURN_TYPE 
      IPC_queryNotifyVC (String msgName, IPC_VARCONTENT_TYPE vc,
			 HANDLER_TYPE handler, Object clientData) {
    return _IPC_queryNotify(msgName, vc.getLength(), vc.getContent(),
			    handler, clientData, false);
  }

  public static IPC_RETURN_TYPE IPC_queryNotifyData (String msgName,
						     Object data,
						     HANDLER_TYPE handler) {
    return IPC_queryNotifyData (msgName, data, handler, null);
  }
  public static IPC_RETURN_TYPE 
      IPC_queryNotifyData (String msgName, Object data,
			   HANDLER_TYPE handler, Object clientData) {
    IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
    IPC_RETURN_TYPE retVal = IPC_RETURN_TYPE.IPC_OK;
    if (data != null) {
      try { retVal = IPC.IPC_marshall(IPC.IPC_msgFormatter(msgName), 
				      coerceDataObject(data), vc);
      } catch ( Exception e) {
	handleException("IPC_queryNotifyData", msgName, e);
      }
    }
    if (retVal == IPC_RETURN_TYPE.IPC_OK) {
      retVal = _IPC_queryNotify(msgName, vc.getLength(), vc.getContent(),
				handler, clientData, true);
      if (vc.getLength() != 0) IPC.IPC_freeByteArray(vc.getContent());
    }
    return retVal;
  }


  public static IPC_RETURN_TYPE IPC_addTimer (long tdelay, int count,
					      TIMER_HANDLER_TYPE handler) {
    return IPC_addTimer (tdelay, count, handler, null);
  }
  public static IPC_RETURN_TYPE
      IPC_addTimer (long tdelay, int count,
		    TIMER_HANDLER_TYPE handler, Object clientData) {
    int handlerNum = ++handlerNumber;
    String hashKey = Integer.toString(handlerNum);

    TIMER_REF_CONTAINER_TYPE timerRef = new TIMER_REF_CONTAINER_TYPE();

    IPC_RETURN_TYPE retVal =  IPC.addTimerGetRef(tdelay, count,
						 (long)handlerNum, timerRef);
    if (retVal == IPC_RETURN_TYPE.IPC_OK) {
      timerHashTable.put(handler.getClass().getName(), hashKey);
      timerHashTable.put(hashKey,
			 new timerHandlerData(handler, timerRef.getTimerRef(),
					      clientData));
    }
    return retVal;
  }

  public static IPC_RETURN_TYPE
      IPC_addTimerGetRef (long tdelay, int count, 
			  TIMER_HANDLER_TYPE handler, Object clientData,
			  TIMER_REF_CONTAINER_TYPE timerRef) {
    int handlerNum = ++handlerNumber;

    IPC_RETURN_TYPE retVal = IPC.addTimerGetRef(tdelay, count, handlerNum,
						timerRef);
    if (retVal == IPC_RETURN_TYPE.IPC_OK) {
      String hashKey = Integer.toString(handlerNum);
      SWIGTYPE_p_void timerRefP = timerRef.getTimerRef();
      timerHashTable.put(Long.toString(SWIGTYPE_p_void.getCPtr(timerRefP)),
			 hashKey);
      timerHashTable.put(hashKey,
			 new timerHandlerData(handler, timerRefP, clientData));
    }
    return retVal;
  }

  public static IPC_RETURN_TYPE
      IPC_addOneShotTimer (long tdelay, TIMER_HANDLER_TYPE handler) {
    return IPC_addOneShotTimer(tdelay, handler, null);
  }
  public static IPC_RETURN_TYPE
      IPC_addOneShotTimer (long tdelay, TIMER_HANDLER_TYPE handler,
			   Object clientData) {
    return IPC.IPC_addTimer(tdelay, 1, handler, clientData);
  }

  public static IPC_RETURN_TYPE 
      IPC_addPeriodicTimer (long tdelay, TIMER_HANDLER_TYPE handler) {
    return IPC_addPeriodicTimer(tdelay, handler, null);
  }
  public static IPC_RETURN_TYPE 
      IPC_addPeriodicTimer (long tdelay, TIMER_HANDLER_TYPE handler,
			   Object clientData) {
    return IPC.IPC_addTimer(tdelay, IPC.TRIGGER_FOREVER, handler, clientData);
  }

  public static IPC_RETURN_TYPE IPC_removeTimer (Class handlerClass) {
    String hashKey = handlerClass.getName();
    String handlerNum = (String)timerHashTable.get(hashKey);
    if (handlerNum == null) {
      System.out.println("Timer for handler ("+hashKey+") does not exist");
      return IPC_RETURN_TYPE.IPC_Error;
    } else {
      timerHandlerData hndData = (timerHandlerData)timerHashTable.get(handlerNum);
      timerHashTable.remove(hashKey);
      timerHashTable.remove(handlerNum);
      return IPC._IPC_removeTimerByRef(hndData.timerRef);
    }
  }

  public static IPC_RETURN_TYPE IPC_removeTimer (TIMER_HANDLER_TYPE handler) {
    return IPC_removeTimer(handler.getClass());
  }

  public static IPC_RETURN_TYPE
      IPC_removeTimerByRef (TIMER_REF_CONTAINER_TYPE timerRef) {
    long cptr = SWIGTYPE_p_void.getCPtr(timerRef.getTimerRef());
    String hashKey = Long.toString(cptr);
    String handlerNum = (String)timerHashTable.get(hashKey);
    if (handlerNum == null) {
      System.out.println("Timer with ref ("+cptr+") does not exist");
      return IPC_RETURN_TYPE.IPC_Error;
    } else {
      timerHashTable.remove(hashKey);
      timerHashTable.remove(handlerNum);
      return IPC._IPC_removeTimerByRef(timerRef.getTimerRef());
    }
  }

  // Change primitive types into types that IPC can handle (same with
  //  the JAVA object versions of those types).
  private static Class coerceDataClass (Class dataClass) {
    if (dataClass == int.class || dataClass == Integer.class) {
      return formatters.IPCInteger.class;
    } else if (dataClass == double.class || dataClass == Double.class) {
      return formatters.IPCDouble.class;
    } else if (dataClass == String.class) {
      return formatters.IPCString.class;
    } else if (dataClass == float.class || dataClass == Float.class) {
      return formatters.IPCFloat.class;
    } else if (dataClass == short.class || dataClass == Short.class) {
      return formatters.IPCShort.class;
    } else if (dataClass == long.class || dataClass == Long.class) {
      return formatters.IPCLong.class;
    } else if (dataClass == char.class) {
      return formatters.IPCChar.class;
    } else if (dataClass == boolean.class || dataClass == Boolean.class) {
      return formatters.IPCBoolean.class;
    } else if (dataClass == byte.class || dataClass == Byte.class) {
      return formatters.IPCByte.class;
    } else {
      return dataClass;
    }
  }

  // Change Java types that "objectize" primitives (plus String)
  //   to types that IPC can handle.
  private static Object coerceDataObject (Object dataObject) {
    if (dataObject == null) return dataObject;

    Class dataClass = dataObject.getClass();

    if (formatters.IPCPrim.class.isAssignableFrom(dataClass)) {
      return dataObject;
    } else if (dataClass == Integer.class) {
      return new formatters.IPCInteger(((Integer)dataObject).intValue());
    } else if (dataClass == Double.class) {
      return new formatters.IPCDouble(((Double)dataObject).intValue());
    } else if (dataClass == String.class) {
      return new formatters.IPCString((String)dataObject);
    } else if (dataClass == Float.class) {
      return new formatters.IPCFloat(((Float)dataObject).floatValue());
    } else if (dataClass == Short.class) {
      return new formatters.IPCShort(((Short)dataObject).shortValue());
    } else if (dataClass == Long.class) {
      return new formatters.IPCLong(((Long)dataObject).longValue());
    } else if (dataClass == Boolean.class) {
      return new formatters.IPCBoolean(((Boolean)dataObject).booleanValue());
    } else if (dataClass == Byte.class) {
      return new formatters.IPCByte(((Byte)dataObject).byteValue());
    } else if (dataClass == Character.class) {
      return new formatters.IPCChar(((char)dataObject));
    } else {
      return dataObject;
    }
  }

}
