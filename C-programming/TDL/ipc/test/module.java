/******************************************************************************
 * PROJECT: New Millennium, DS1
 *          IPC (Interprocess Communication) Package
 *
 * (c) Copyright 2002 Reid Simmons.  All rights reserved.
 *
 * FILE: module.java
 *
 * ABSTRACT: Public class file for module1, module2 and module3 test programs
 *
 * $Revision: 2.3 $
 * $Date: 2013/07/24 20:01:01 $
 * $Author: reids $
 *
 * Copyright (c) 2008, Carnegie Mellon University
 *     This software is distributed under the terms of the 
 *     Simplified BSD License (see ipc/LICENSE.TXT)
 *
 * REVISION HISTORY
 *
 * $Log: module.java,v $
 * Revision 2.3  2013/07/24 20:01:01  reids
 * Updating lisp, java, python test programs to adhere to updated API
 *
 * Revision 2.2  2009/01/12 15:54:58  reids
 * Added BSD Open Source license info
 *
 * Revision 2.1  2002/01/02 21:13:11  reids
 * Added test files for the Java version of IPC, corresponding to the
 *   C and Lisp test files that already existed.
 *
 ****************************************************************/

public class module {
  /* STATUS_ENUM */
  public enum Status {WaitVal, SendVal, ReceiveVal, ListenVal}

  private static String statusToString (Status status) {
    return (status == Status.WaitVal ? "WaitVal"
	    : status == Status.SendVal ? "SendVal"
	    : status == Status.ReceiveVal ? "ReceiveVal"
	    : status == Status.ListenVal ? "ListenVal" : "???");
  }

  public static class T1 {
    public int i1;
    public Status status;
    public double matrix[/*2*/][/*3*/];
    public double d1;
    public String toString () {
      String str = "{" + i1 +", "+ statusToString(status) +", [";
      for (int i=0; i<matrix.length; i++) {
	str += "[";
	for (int j=0; j<matrix[i].length; j++) {
	  str += matrix[i][j];
	  if (j != matrix[i].length-1) str += ", ";
	}
	str += "]";
	if (i != matrix.length-1) str += ", ";
      }
      str += "]";
      return str +", "+ d1 +"}";
    }
  }

  protected static final String T1_NAME  = "T1";
  // First form of "enum". 3 is the maximum value -- i.e., the value of WaitVal
  protected static final String  T1_FORMAT =
      "{int, {enum : 3}, [double:2,3], double}";

  public static class T2 {
    public String str1;
    public int count;
    public T1 t1[]; /* Variable length array of type T1_TYPE */
    public Status status;

    public String toString () {
      String str = "{\"" + str1 +"\", "+ count +", ";
      str += "<";
      for (int i = 0; i<count; i++) str += t1[i].toString();
      return str + ">, " + statusToString(status) +"]";
    }
  }

  protected static final String  T2_NAME = "T2";
  // Alternate form of "enum".
  protected static final String  T2_FORMAT =
      "{string, int, <T1:2>, {enum WaitVal, SendVal, ReceiveVal, ListenVal}}";

  protected static final String  MSG1        = "message1";
  protected static final String  MSG1_FORMAT = "int";

  protected static final String  MSG2        = "message2";
  protected static final String  MSG2_FORMAT = "string";

  protected static final String  QUERY1        = "query1";
  protected static final String  QUERY1_FORMAT = T1_NAME;

  protected static final String  RESPONSE1        = "response1";
  protected static final String  RESPONSE1_FORMAT = T2_NAME;

  protected static final String  MODULE1_NAME = "module1";
  protected static final String  MODULE2_NAME = "module2";
  protected static final String  MODULE3_NAME = "module3";
}
