/******************************************************************************
 * PROJECT: New Millennium, DS1
 *          IPC (Interprocess Communication) Package
 *
 * (c) Copyright 2002 Reid Simmons.  All rights reserved.
 *
 * FILE: module3.java
 *
 * ABSTRACT: Test program for IPC.
 *             Subscribes to: MSG1, MSG2
 *             Behavior: Prints out the message data received.
 *                       Exits when 'q' is typed at terminal
 *                       Should be run in conjunction with module1 and module2
 *
 * $Revision: 2.4 $
 * $Date: 2013/07/24 20:01:01 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * REVISION HISTORY
 *
 * $Log: module3.java,v $
 * Revision 2.4  2013/07/24 20:01:01  reids
 * Updating lisp, java, python test programs to adhere to updated API
 *
 * Revision 2.3  2009/09/04 19:09:27  reids
 * IPC Java is now in its own package
 *
 * Revision 2.2  2009/01/12 15:54:58  reids
 * Added BSD Open Source license info
 *
 * Revision 2.1  2002/01/02 21:13:10  reids
 * Added test files for the Java version of IPC, corresponding to the
 *   C and Lisp test files that already existed.
 *
 ****************************************************************/

import ipc.java.*;

public class module3 extends module {
  private static class msg1Handler implements IPC.HANDLER_TYPE {
    public void handle (MSG_INSTANCE msgRef, Object callData, 
			Object clientData) {
      System.out.println("msg1Handler: Receiving "+
			 IPC.IPC_msgInstanceName(msgRef) +" ("+ callData
			 +") ["+ clientData +"]");
    }
  }

  private static class msg2Handler implements IPC.HANDLER_TYPE {
    public void handle (MSG_INSTANCE msgRef, Object callData, 
			Object clientData) {
      System.out.println("msg2Handler: Receiving "+
			 IPC.IPC_msgInstanceName(msgRef) +" (\""+ callData
			 +"\") ["+ clientData +"]");
    }
  }

  private static class stdinHnd implements IPC.FD_HANDLER_TYPE {
    public void handle (int fd, Object clientData) {
      try {
	  int in = System.in.read();

	  if (in == 'q' || in == 'Q') {
	    IPC.IPC_disconnect();
	    System.exit(-1);
	  } else {
	    System.out.println("stdinHnd ["+ clientData +"]: Received "+ 
			       (char)in);
	  }
	  // Read in any extra bytes
	while (System.in.available() > 0) System.in.read();
      } catch (Exception e) { e.printStackTrace(); }
    }
  }

  public static void main (String args[]) throws Exception {
    /* Connect to the central server */
    System.out.println("\nIPC.IPC_connect(\""+ MODULE3_NAME +"\")");
    IPC.IPC_connect(MODULE3_NAME);

    /* Subscribe to the messages that this module listens to. */
    System.out.println("\nIPC.IPC_subscribeData(\""+ MSG1 +"\", new msg1Handler(\""+
		       MODULE3_NAME +"\"), int.class)");
    IPC.IPC_msgClass(MSG1, int.class);
    IPC.IPC_subscribeData(MSG1, new msg1Handler(), MODULE3_NAME);

    System.out.println("\nIPC.IPC_subscribeData(\""+ MSG2 +"\", new msg2Handler(\""+
		       MODULE3_NAME +"\"), String.class)");
    IPC.IPC_msgClass(MSG2, String.class);
    IPC.IPC_subscribeData(MSG2, new msg2Handler(), MODULE3_NAME);

    /* Subscribe a handler for tty input. Typing "q" will quit the program. */
    System.out.println("\nIPC_subscribeFD(0, new stdinHnd(\""+ 
		       MODULE3_NAME +"\"))");
    IPC.IPC_subscribeFD(0, new stdinHnd(), MODULE3_NAME);

    System.out.println("\nType 'q' to quit");
    IPC.IPC_dispatch();

    IPC.IPC_disconnect();
  }
}

