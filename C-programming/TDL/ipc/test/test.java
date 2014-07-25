// To run:
// gmake install in $IPC_DIR/java
// export LD_LIBRARY_PATH="$IPC_DIR/lib/$SYS"
//  where IPC_DIR is the location of IPC and SYS is the system type
//  (e.g., Linux-3.8)
// javac -classpath ../java/build test.java
// Run $IPC_DIR/bin/$SYS/central in a separate terminal
// export CENTRALHOST=localhost
// java -classpath ../java/build:. test
// Follow the instructions (you need to enter input and start/stop the
//  $IPC_DIR/test/module2 program (which needs to be built first)
// You can then compare the output against test.java.output.
// Except for differences in pointer values, and occasional swaps in which
// handler fires first, they should be identical

import ipc.java.*;

public class test {
    private static final boolean printByteArrayP = true;

    private static void printVC (IPC_VARCONTENT_TYPE vc) {
      if (printByteArrayP) {
	IPC.printByteArray(vc.getContent(), (int)vc.getLength());
      }
    }

    public static class sample0 {
	sample0 (double _d1, double _d2) { d1 = _d1; d2 = _d2; }
	public double d1 = 1;
	public double d2 = 2;
    }

    public static class sample1 {
	public char c1, c2;
    }

    public static void test0 () throws Exception {
      IPC.IPC_initialize();
      BUFFER_TYPE buf = IPC.createBuffer(IPC.createByteArray(10));
      sample0 ds = new sample0(666, 0);
      primFmttrs.Encode(primFmttrs.DOUBLE_FMT, ds, 0, buf);
      IPC.printBuffer(buf);
      IPC.rewindBuffer(buf);
      primFmttrs.Decode(primFmttrs.DOUBLE_FMT, ds, 1, buf);
      System.out.println(ds.d2);
      System.out.println(primFmttrs.ELength(primFmttrs.DOUBLE_FMT, ds, 1));
      System.out.println(primFmttrs.SimpleType(primFmttrs.DOUBLE_FMT));
      double[] ar = new double[3];
      ar[0] = 1.0; ar[1] = 2.0; ar[2] = 3.0;
      IPC.rewindBuffer(buf);
      primFmttrs.EncodeElement(primFmttrs.DOUBLE_FMT, ar, 1, buf);
      IPC.printBuffer(buf);
      IPC.rewindBuffer(buf);
      primFmttrs.DecodeElement(primFmttrs.DOUBLE_FMT, ar, 2, buf);
      System.out.println("["+ar[0] +", "+ ar[1] +", "+ ar[2]+"]");
    }

    public static void test1 () throws Exception {
      IPC.IPC_initialize();
      BUFFER_TYPE buf = IPC.createBuffer(IPC.createByteArray(10));
      sample1 ds = new sample1(); ds.c1 = 'h';
      primFmttrs.Encode(primFmttrs.CHAR_FMT, ds, 0, buf);
      IPC.printBuffer(buf);
      IPC.rewindBuffer(buf);
      primFmttrs.Decode(primFmttrs.CHAR_FMT, ds, 1, buf);
      System.out.println(ds.c2);
      System.out.println(primFmttrs.ELength(primFmttrs.CHAR_FMT, ds, 1));
      System.out.println(primFmttrs.SimpleType(primFmttrs.CHAR_FMT));
      char[] ar = new char[3];
      ar[0] = 'e'; ar[1] = 'm'; ar[2] = 'c';
      IPC.rewindBuffer(buf);
      primFmttrs.EncodeElement(primFmttrs.CHAR_FMT, ar, 1, buf);
      IPC.printBuffer(buf);
      IPC.rewindBuffer(buf);
      primFmttrs.DecodeElement(primFmttrs.CHAR_FMT, ar, 2, buf);
      System.out.println("["+ar[0] +", "+ ar[1] +", "+ ar[2]+"]");
    }

    // This tests all the primitives
    public static void test2 () throws Exception {
      IPC.IPC_initialize();
      FORMAT_TYPE fmt1 = IPC.IPC_parseFormat("int");
      IPC_VARCONTENT_TYPE vc1 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt1, 123, vc1);
      printVC(vc1);
      System.out.println(IPC.IPC_unmarshallData(fmt1, vc1.getContent(), 
						int.class));

      FORMAT_TYPE fmt3 = IPC.IPC_parseFormat("boolean");
      IPC_VARCONTENT_TYPE vc3 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt3, true, vc3);
      printVC(vc3);
      System.out.println(IPC.IPC_unmarshallData(fmt3, vc3.getContent(),
						boolean.class));

      FORMAT_TYPE fmt4 = IPC.IPC_parseFormat("float");
      IPC_VARCONTENT_TYPE vc4 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt4, (float)55.0, vc4);
      printVC(vc4);
      System.out.println(IPC.IPC_unmarshallData(fmt4, vc4.getContent(),
						float.class));

      FORMAT_TYPE fmt2 = IPC.IPC_parseFormat("double");
      IPC_VARCONTENT_TYPE vc2 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt2, 666.0, vc2);
      printVC(vc2);
      System.out.println(IPC.IPC_unmarshallData(fmt2, vc2.getContent(),
						double.class));

      FORMAT_TYPE fmt5 = IPC.IPC_parseFormat("byte");
      IPC_VARCONTENT_TYPE vc5 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt5, (byte)0xA, vc5);
      printVC(vc5);
      System.out.println(IPC.IPC_unmarshallData(fmt5, vc5.getContent(),
						byte.class));

      fmt5 = IPC.IPC_parseFormat("byte");
      vc5 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt5, (byte)0xFA, vc5);
      printVC(vc5);
      System.out.println(IPC.IPC_unmarshallData(fmt5, vc5.getContent(),
						byte.class));

      FORMAT_TYPE fmt6 = IPC.IPC_parseFormat("string");
      IPC_VARCONTENT_TYPE vc6 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt6, "hello", vc6);
      printVC(vc6);
      System.out.println(IPC.IPC_unmarshallData(fmt6, vc6.getContent(),
						String.class));

      fmt6 = IPC.IPC_parseFormat("string");
      vc6 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt6, "", vc6);
      printVC(vc6);
      System.out.println(IPC.IPC_unmarshallData(fmt6, vc6.getContent(),
						String.class));
      /*
      fmt6 = IPC.IPC_parseFormat("string");
      vc6 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt6, null, vc6);
      printVC(vc6);
      System.out.println(IPC.IPC_unmarshallData(fmt6, vc6.getContent(),
						String.class));
      */
      FORMAT_TYPE fmt7 = IPC.IPC_parseFormat("char");
      IPC_VARCONTENT_TYPE vc7 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt7, 'c', vc7);
      printVC(vc7);
      System.out.println(IPC.IPC_unmarshallData(fmt7, vc7.getContent(),
						char.class));

      FORMAT_TYPE fmt8 = IPC.IPC_parseFormat("short");
      IPC_VARCONTENT_TYPE vc8 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt8, (short)666, vc8);
      printVC(vc8);
      System.out.println(IPC.IPC_unmarshallData(fmt8, vc8.getContent(),
						short.class));

      FORMAT_TYPE fmt9 = IPC.IPC_parseFormat("long");
      IPC_VARCONTENT_TYPE vc9 = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(fmt9, (long)0X7FFFFFFF, vc9);
      printVC(vc9);
      System.out.println(IPC.IPC_unmarshallData(fmt9, vc9.getContent(),
						long.class));
    }

    public static class struct1 {
      public struct1() {}
      public struct1(int _i, struct2 _a1) { i = _i; a1 = _a1; }
      public int i;
      public struct2 a1;
    }

    public static class struct2 {
      public struct2() {}
      public struct2(String _str, double _d) { str = _str; d = _d; }
      public String str;
      public double d;
    }

    public static void test3 () throws Exception {
      IPC.IPC_initialize();
      FORMAT_TYPE fmt1 = IPC.IPC_parseFormat("{int, {string, double}}");
      IPC_VARCONTENT_TYPE vc1 = new IPC_VARCONTENT_TYPE();
      struct1 ds = new struct1(666, new struct2("hello", 3.14159));
      IPC.IPC_marshall(fmt1, ds, vc1);
      printVC(vc1);
      ds = (struct1)IPC.IPC_unmarshallData(fmt1, vc1.getContent(),
					   struct1.class);
      System.out.println("{"+ds.i+", {"+ds.a1.str+", "+ds.a1.d+"}}");

      struct2 ds2 = new struct2("eat more spam", 9.87654321);
      struct1 ds1 = new struct1(1234, ds2), ds1a = new struct1();
      IPC.IPC_marshall(fmt1, ds1, vc1);
      printVC(vc1);
      IPC_RETURN_TYPE ret = IPC.IPC_unmarshall(fmt1, vc1.getContent(), ds1a);
      System.out.println("{"+ds1a.i+", {"+ds1a.a1.str+", "+ds1a.a1.d+"}}, "+
			 ret+", "+ (ds1==ds1a)+", "+ (ds2==ds1a.a1));

      // Should raise an error
      try { IPC.IPC_unmarshallData(fmt1, vc1.getContent(), struct2.class); }
      catch (Exception e) { System.err.println("ERROR: "+e.getMessage()); }
    }

    // test fixed arrays
    public static void test4 () throws Exception {
      IPC.IPC_initialize();
      FORMAT_TYPE fmt1 = IPC.IPC_parseFormat("[int : 5]");
      IPC_VARCONTENT_TYPE vc1 = new IPC_VARCONTENT_TYPE();
      int [] ds1 = new int[5], ds1a = new int[5];
      for (int i=0; i<5; i++) ds1[i] = i+10;
      IPC.IPC_marshall(fmt1, ds1, vc1);
      printVC(vc1);
      IPC.IPC_unmarshall(fmt1, vc1.getContent(), ds1a);
      System.out.print("[");
      for (int i=0; i<5; i++) 
	System.out.print((i > 0 ? ", " : "")+ds1a[i]);
      System.out.println("]");

      FORMAT_TYPE fmt2 = IPC.IPC_parseFormat("[{string, double} : 5]");
      IPC_VARCONTENT_TYPE vc2 = new IPC_VARCONTENT_TYPE();
      struct2 [] ds2 = new struct2[5], ds2a;
      String [] str = {"eat", "more", "spam", "for", "life"};
      for (int i=0; i<5; i++) ds2[i] = new struct2(str[i], Math.pow(i, 3));
      IPC.IPC_marshall(fmt2, ds2, vc2);
      printVC(vc2);
      ds2a = (struct2 [])IPC.IPC_unmarshallData(fmt2, vc2.getContent(),
						struct2[].class);
      System.out.print("[");
      for (int i=0; i<5; i++) 
	System.out.print((i>0 ? ", " : "")+"{"+ds2a[i].str+", "+ds2a[i].d+"}");
      System.out.println("]");

      FORMAT_TYPE fmt3 = IPC.IPC_parseFormat("[int : 3, 4]");
      IPC_VARCONTENT_TYPE vc3 = new IPC_VARCONTENT_TYPE();
      int [][] ds3 = new int[3][4], ds3a = new int[3][4];
      for (int i=0; i<3; i++) for (int j=0; j<4; j++) 
	ds3[i][j] = (int)(Math.pow(i+1, 2)+j);
      IPC.IPC_marshall(fmt3, ds3, vc3);
      printVC(vc3);
      IPC.IPC_unmarshall(fmt3, vc3.getContent(), ds3a);
      System.out.print("[");
      for (int i=0; i<3; i++) {
	System.out.print("[");
	for (int j=0; j<4; j++)
	  System.out.print((j > 0 ? ", " : "")+ds3a[i][j]);
	System.out.print("]");
      }
      System.out.println("]");

      FORMAT_TYPE fmt4 = IPC.IPC_parseFormat("[double : 3, 4]");
      IPC_VARCONTENT_TYPE vc4 = new IPC_VARCONTENT_TYPE();
      double [][] ds4 = new double[3][4], ds4a;
      for (int i=0; i<3; i++) for (int j=0; j<4; j++) 
	ds4[i][j] = (int)(Math.pow(i+1, 2) + Math.pow(j+1, 2)+4);
      IPC.IPC_marshall(fmt4, ds4, vc4);
      printVC(vc4);
      ds4a = (double [][])IPC.IPC_unmarshallData(fmt4, vc4.getContent(),
						 double[][].class);
      System.out.print("[");
      for (int i=0; i<3; i++) {
	System.out.print("[");
	for (int j=0; j<4; j++)
	  System.out.print((j > 0 ? ", " : "")+ds4a[i][j]);
	System.out.print("]");
      }
      System.out.println("]");
    }

    public static class struct4 {
      public int num;
      public int[] ar;
    }

    public static class struct5 {
      public struct2[] ar;
      public int num;
    }

    public static class struct6 {
      public int dim1;
      public int dim2;
      public int[][] ar;
    }

    // test variable arrays
    public static void test5 () throws Exception {
      IPC.IPC_initialize();
      FORMAT_TYPE fmt1 = IPC.IPC_parseFormat("{int, <int : 1>}");
      IPC_VARCONTENT_TYPE vc1 = new IPC_VARCONTENT_TYPE();
      struct4 ds1 = new struct4(), ds1a = new struct4();
      ds1.num = 5; ds1.ar = new int[5];
      for (int i=0; i<5; i++) ds1.ar[i] = i+111;
      IPC.IPC_marshall(fmt1, ds1, vc1);
      printVC(vc1);
      IPC.IPC_unmarshall(fmt1, vc1.getContent(), ds1a);
      System.out.print("{"+ds1a.num+", [");
      for (int i=0; i<5; i++) 
	System.out.print((i > 0 ? ", " : "")+ds1a.ar[i]);
      System.out.println("]}");

      FORMAT_TYPE fmt2 = IPC.IPC_parseFormat("{<{string, double} : 2>, int}");
      IPC_VARCONTENT_TYPE vc2 = new IPC_VARCONTENT_TYPE();
      struct5 ds2 = new struct5(), ds2a;
      String [] str = {"eat", "more", "spam", "for", "life"};
      ds2.num = 5; ds2.ar = new struct2[5];
      for (int i=0; i<5; i++) ds2.ar[i] = new struct2(str[i], Math.pow(i, 3));
      IPC.IPC_marshall(fmt2, ds2, vc2);
      printVC(vc2);
      ds2a = (struct5)IPC.IPC_unmarshallData(fmt2, vc2.getContent(),
					     struct5.class);
      System.out.print("{"+ds2a.num+", [");
      for (int i=0; i<5; i++) 
	System.out.print((i > 0 ? ", " : "")+"{"+ds2a.ar[i].str+", "+ds2a.ar[i].d+"}");
      System.out.println("]}");

      FORMAT_TYPE fmt3 = IPC.IPC_parseFormat("{int, int, <int : 1, 2>}");
      IPC_VARCONTENT_TYPE vc3 = new IPC_VARCONTENT_TYPE();
      struct6 ds3 = new struct6(), ds3a = new struct6(), ds3b;
      ds3.dim1 = 3; ds3.dim2 = 4; ds3.ar = new int[3][4];
      for (int i=0; i<3; i++) for (int j=0; j<4; j++) 
	ds3.ar[i][j] = (int)Math.pow(i+1,2)+j;
      IPC.IPC_marshall(fmt3, ds3, vc3);
      printVC(vc3);
      IPC.IPC_unmarshall(fmt3, vc3.getContent(), ds3a);
      System.out.print("{"+ds3a.dim1+", "+ds3a.dim2+", [");
      System.out.print("[");
      for (int i=0; i<3; i++) {
	System.out.print("[");
	for (int j=0; j<4; j++)
	  System.out.print((j > 0 ? ", " : "")+ds3a.ar[i][j]);
	System.out.print("]");
      }
      System.out.println("]}");
      ds3b = (struct6)IPC.IPC_unmarshallData(fmt3, vc3.getContent(),
					     struct6.class);
      System.out.print("{"+ds3b.dim1+", "+ds3b.dim2+", [");
      System.out.print("[");
      for (int i=0; i<3; i++) {
	System.out.print("[");
	for (int j=0; j<4; j++)
	  System.out.print((j > 0 ? ", " : "")+ds3b.ar[i][j]);
	System.out.print("]");
      }
      System.out.println("]}");
    }

    public enum Status {WaitVal, SendVal, ReceiveVal, ListenVal}
    public static class struct7 {
      public int i1;
      public Status status;
    }

    // test enums
    public static void test6 () throws Exception {
      IPC.IPC_initialize();

      FORMAT_TYPE fmt1 = IPC.IPC_parseFormat("{int, {enum WaitVal, SendVal, ReceiveVal, ListenVal}}");
      IPC_VARCONTENT_TYPE vc1 = new IPC_VARCONTENT_TYPE();
      struct7 ds1 = new struct7(), ds1a = new struct7();
      ds1.i1 = 42; ds1.status = Status.ReceiveVal;
      IPC.IPC_marshall(fmt1, ds1, vc1);
      printVC(vc1);
      IPC.IPC_unmarshall(fmt1, vc1.getContent(), ds1a);
      System.out.println("{"+ds1a.i1+", "+ds1a.status+"}");

      FORMAT_TYPE fmt2 = IPC.IPC_parseFormat("[{enum WaitVal, SendVal, ReceiveVal, ListenVal}:3]");
      IPC_VARCONTENT_TYPE vc2 = new IPC_VARCONTENT_TYPE();
      Status [] ds2 = new Status[3];
      ds2[0] = Status.SendVal; ds2[1] = Status.ListenVal;
      ds2[2] = Status.WaitVal;
      IPC.IPC_marshall(fmt2, ds2, vc2);
      printVC(vc2);
      Status [] ds2a = (Status[])IPC.IPC_unmarshallData(fmt2, vc2.getContent(),
							Status[].class);
      System.out.print("[");
      for (int i=0; i<3; i++) 
	System.out.print((i > 0 ? ", " : "")+ds2a[i]);
      System.out.println("]");

      FORMAT_TYPE fmt3 = IPC.IPC_parseFormat("{int, {enum :4}}");
      IPC_VARCONTENT_TYPE vc3 = new IPC_VARCONTENT_TYPE();
      struct7 ds3 = new struct7(), ds3a;
      ds3.i1 = 42; ds3.status = Status.ReceiveVal;
      IPC.IPC_marshall(fmt3, ds3, vc3);
      printVC(vc3);
      ds3a = (struct7)IPC.IPC_unmarshallData(fmt3, vc3.getContent(),
					     struct7.class);
      System.out.println("{"+ds3a.i1+", "+ds3a.status+"}");
    }
      
    // test pointers
    public static void test7 () throws Exception {
      IPC.IPC_initialize();

      FORMAT_TYPE fmt1 = IPC.IPC_parseFormat("{int, {string, double}}");
      IPC_VARCONTENT_TYPE vc1 = new IPC_VARCONTENT_TYPE();
      struct1 ds1 = new struct1(666, new struct2("hello", 3.14159)), 
	  ds1a = new struct1();
      IPC.IPC_marshall(fmt1, ds1, vc1);
      printVC(vc1);
      IPC.IPC_unmarshall(fmt1, vc1.getContent(), ds1a);
      System.out.println("{"+ds1.i+", {"+ds1.a1.str+", "+ds1.a1.d+"}}");

      FORMAT_TYPE fmt2 = IPC.IPC_parseFormat("{*int, *{string, double}}");
      IPC_VARCONTENT_TYPE vc2 = new IPC_VARCONTENT_TYPE();
      struct1 ds2 = new struct1(666, new struct2("hello", 3.14159)), 
	  ds2a = new struct1();
      IPC.IPC_marshall(fmt2, ds2, vc2);
      printVC(vc2);
      IPC.IPC_unmarshall(fmt2, vc2.getContent(), ds2a);
      System.out.println("{"+ds2.i+", {"+ds2.a1.str+", "+ds2.a1.d+"}}");

      FORMAT_TYPE fmt3 = fmt2;
      IPC_VARCONTENT_TYPE vc3 = new IPC_VARCONTENT_TYPE();
      struct1 ds3 = new struct1(666, null), ds3a;
      IPC.IPC_marshall(fmt3, ds3, vc3);
      printVC(vc3);
      ds3a = (struct1)IPC.IPC_unmarshallData(fmt3, vc3.getContent(),
					     struct1.class);
      System.out.println("{"+ds3.i+", "+ds3.a1+"}");
    }

    public static void test8 () throws Exception {
      IPC.IPC_initialize();
      System.out.println(IPC.IPC_parseFormat(""));
      System.out.println(IPC.IPC_parseFormat(null));
    }

    private static class msgHandler1 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object data,
			  Object clientData) {
	System.out.println("msgHandler1: "+IPC.IPC_msgInstanceName(msgInstance)+
			   " "+data+" "+clientData);
      }
    }

    private static class msgHandler2 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object data,
			  Object clientData) {
        System.out.println("msgHandler2: "+IPC.IPC_msgInstanceName(msgInstance)+
			   " "+data+" "+clientData+
			   " "+IPC.IPC_dataLength(msgInstance));
      }
    }

    public static void test9 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_defineMsg("f", IPC.IPC_VARIABLE_LENGTH, "int");
      IPC.IPC_msgClass("f", Integer.class);
      IPC.IPC_subscribeData("f", new msgHandler1(), 1);
      IPC.IPC_listenWait(100); // Seems to be a timing issue here, sometimes
      IPC.IPC_publishData("f", 42);
      IPC.IPC_listenWait(500);
      System.out.println();

      IPC.IPC_subscribeData("f", new msgHandler2(), 3);
      IPC.IPC_publishData("f", 666);
      IPC.IPC_listenWait(500);
      System.out.println();

      IPC.IPC_subscribeData("f", new msgHandler1(), 2);
      System.out.println("Num handlers: "+IPC.IPC_numHandlers("f"));
      IPC.IPC_publishData("f", 1234);
      IPC.IPC_listenWait(500);
      System.out.println();

      IPC.IPC_unsubscribe("f", new msgHandler2());
      System.out.println("Num handlers: "+IPC.IPC_numHandlers("f"));
      IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(IPC.IPC_msgFormatter("f"), 4321, vc);
      IPC.IPC_publishVC("f", vc);
      IPC.IPC_listenWait(500);

      IPC.IPC_disconnect();
    }

    private static boolean exit = false;

    public static class stdinHnd implements IPC.FD_HANDLER_TYPE {
      public void handle (int fd, Object clientData) {
	try {
	  // Probably a much simpler way of doing this...
	  String msg = ""; int in;
	  while ((in = System.in.read()) != -1 && (in != (int)'\n'))
	    msg += (char)in;
	  char c = msg.charAt(0);
	  if (c == '?' || c == 'h') {
	    System.out.println("h: help");
	    System.out.println("q: quit");
	    System.out.println("u: stop listening");
	  } else if (c == 'q') {
	    System.out.println("quit");
	    exit = true;
	  } else if  (c == 'u') {
	    System.out.println("silent");
	    IPC.IPC_unsubscribeFD(fd, this);
	  } else {
	    System.out.println("Unhandled input: "+msg);
	  }
	} catch (Exception e) { e.printStackTrace(); }
      }
    }

    public static void test10 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_subscribeFD(0, new stdinHnd(), null);
      exit = false;
      System.out.println("Please type -- either 'h' or 'q', end with a 'q'");
      while (!exit) IPC.IPC_listen(1000);
      IPC.IPC_disconnect();
    }

    public static void test11 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_perror("Test");
      IPC.IPC_setCapacity(3);
      IPC.IPC_defineMsg("f", IPC.IPC_VARIABLE_LENGTH, "int");
      IPC.IPC_setMsgQueueLength("f", 1);
      IPC.IPC_setMsgPriority("f", 2);
      IPC.IPC_setVerbosity(IPC_VERBOSITY_TYPE.IPC_Print_Errors);
      IPC.IPC_disconnect();
      IPC.IPC_defineMsg("h", IPC.IPC_VARIABLE_LENGTH, "int");
      System.out.println("HERE");
      IPC.IPC_disconnect();
    }

    public static void test12 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_defineMsg("f", IPC.IPC_VARIABLE_LENGTH, "int");
      IPC.IPC_defineMsg("h", IPC.IPC_VARIABLE_LENGTH, "int");
      System.out.println("Here1");
      IPC.IPC_subscribeData("f", new msgHandler1(), 1);
      System.out.println("Here2");
      IPC.IPC_subscribeData("f", new msgHandler1(), 2);
      System.out.println("Here3");
      IPC.IPC_subscribeData("g", new msgHandler1(), 1);
      System.out.println("Here4");
      IPC.IPC_disconnect();
    }

    private static class msgHandler3 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object byteArray,
			  Object clientData) {
	try {
	  int data = (int)IPC.IPC_unmarshallData(IPC.IPC_msgFormatter("g"),
						 (SWIGTYPE_p_void)byteArray,
						 int.class);
	  System.out.println("msgHandler3: "+
			     IPC.IPC_msgInstanceName(msgInstance)+" "+data+" "+
			     clientData+" "+IPC.IPC_dataLength(msgInstance));
	} catch (Exception e) { 
	  System.err.println("ERROR:"+e.getMessage()); e.printStackTrace(); }
	IPC.IPC_freeByteArray((SWIGTYPE_p_void)byteArray);
      }
    }

    public static void test13 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_defineMsg("f", 4, null);
      IPC.IPC_subscribe("f", new msgHandler3()); // optional client data
      IPC.IPC_defineMsg("g", IPC.IPC_VARIABLE_LENGTH, "int");
      IPC.IPC_listenWait(100); // Seems to be a timing issue here, sometimes
      IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
      IPC.IPC_marshall(IPC.IPC_parseFormat("int"), 1234, vc);
      IPC.IPC_publish("f", vc.getLength(), vc.getContent());
      IPC.IPC_listenWait(500);
      IPC.IPC_publishVC("f", vc);
      IPC.IPC_listenWait(500);
      IPC.IPC_publishFixed("f", vc.getContent());
      IPC.IPC_listenWait(500);
      IPC.IPC_disconnect();
    }

    private static boolean connected = false;
    private static boolean disconnected = false;

    private static class connectHandler1 implements IPC.CONNECT_HANDLER_TYPE {
      public void handle (String moduleName, Object clientData) {
	connected = true;
	System.out.println("connectHandler1: "+moduleName+" "+clientData);
      }
    }

    private static class connectHandler2 implements IPC.CONNECT_HANDLER_TYPE {
      public void handle (String moduleName, Object clientData) {
	System.out.println("connectHandler2: "+moduleName+" "+clientData);
      }
    }

    private static class disconnectHandler1 implements IPC.CONNECT_HANDLER_TYPE {
      public void handle (String moduleName, Object clientData) {
	disconnected = true;
	System.out.println("disconnectHandler1: "+moduleName+" "+clientData);
      }
    }

    private static class disconnectHandler2 implements IPC.CONNECT_HANDLER_TYPE {
      public void handle (String moduleName, Object clientData) {
	System.out.println("disconnectHandler2: "+moduleName+" "+clientData);
      }
    }

    public static void test14 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_subscribeConnect(new connectHandler1(), null);
      IPC.IPC_subscribeDisconnect(new disconnectHandler1(), null);
      connected = false; disconnected = false;
      System.out.println("Please start, then quit from, test/module2");
      while (!connected || !disconnected) IPC.IPC_listen(1000);
      System.out.println("HERE1");

      IPC.IPC_subscribeConnect(new connectHandler1(), 1);
      IPC.IPC_subscribeConnect(new connectHandler2(), null);
      IPC.IPC_subscribeDisconnect(new disconnectHandler1(), 1);
      IPC.IPC_subscribeDisconnect(new disconnectHandler2(), null);
      connected = false; disconnected = false;
      System.out.println("Please start, then quit from, test/module2");
      while (!connected || !disconnected) IPC.IPC_listen(1000);
      System.out.println("HERE2");

      IPC.IPC_unsubscribeConnect(connectHandler2.class);
      IPC.IPC_unsubscribeDisconnect(new disconnectHandler2());
      connected = false; disconnected = false;
      System.out.println("Please start, then quit from, test/module2");
      while (!connected || !disconnected) IPC.IPC_listen(1000);
      System.out.println("HERE3");

      IPC.IPC_unsubscribeConnect(connectHandler1.class);
      connected = true; disconnected = false;
      System.out.println("Please start, then quit from, test/module2");
      System.out.println("  (shouldn't do anything on connection, this time)");
      while (!connected || !disconnected) IPC.IPC_listen(1000);
      System.out.println("HERE4");
      IPC.IPC_disconnect();
    }

    private static boolean changed = false;

    private static class changeHandler1 implements IPC.CHANGE_HANDLER_TYPE {
	public void handle (String msgName, int numHandlers,
			    Object clientData) {
	changed = true;
	System.out.println("changeHandler1: "+
			   msgName+" "+numHandlers+" "+clientData);
      }
    }

    private static class changeHandler2 implements IPC.CHANGE_HANDLER_TYPE {
	public void handle (String msgName, int numHandlers,
			    Object clientData) {
	System.out.println("changeHandler2: "+
			   msgName+" "+numHandlers+" "+clientData);
      }
    }

    public static void test15 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_defineMsg("message1", IPC.IPC_VARIABLE_LENGTH, "int");
      IPC.IPC_subscribeHandlerChange("message1", new changeHandler1(), null);
      changed = false;
      System.out.println("Please start test/module2");
      while (!changed) IPC.IPC_listen(1000);
      System.out.println("HERE1");

      IPC.IPC_subscribeHandlerChange("message1", new changeHandler1(), 1);
      IPC.IPC_subscribeHandlerChange("message1", new changeHandler2(), null);
      changed = false;
      System.out.println("Please quit from test/module2");
      while (!changed) IPC.IPC_listen(1000);
      System.out.println("HERE2");

      IPC.IPC_unsubscribeHandlerChange("message1", changeHandler2.class);
      changed = false;
      System.out.println("Please start test/module2");
      while (!changed) IPC.IPC_listen(1000);
      System.out.println("HERE3");

      IPC.IPC_subscribeDisconnect(new disconnectHandler1(), 123);
      IPC.IPC_unsubscribeHandlerChange("message1", changeHandler1.class);
      disconnected = false;
      System.out.println("Please quit from test/module2");
      System.out.println("  (shouldn't call the change handler, this time)");
      while (!disconnected) IPC.IPC_listen(1000);
      System.out.println("HERE4");
      IPC.IPC_disconnect();
    }

    // test named formatters
    // Need to be connected to IPC central to test this...
    public static void test16 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_defineMsg("h", IPC.IPC_VARIABLE_LENGTH, "{int, fooType}");
      IPC.IPC_defineFormat("fooType", "{string, double}");
      IPC.IPC_subscribeData("h", new msgHandler2(), null);
      struct1 ds1 = new struct1(666, new struct2("hello", 3.14159));
      IPC.IPC_setVerbosity(IPC_VERBOSITY_TYPE.IPC_Print_Errors);
      System.out.println("Do formats match? {int, {string, double}}: "+
			 IPC.IPC_checkMsgFormats("h", "{int, {string, double}}"));
      System.out.println("Do formats match? {int, fooType}: "+
			 IPC.IPC_checkMsgFormats("h", "{int, fooType}"));
      System.out.println("Do formats match? {int, double}: "+
			 IPC.IPC_checkMsgFormats("h", "{int, double}"));
      IPC.IPC_listenWait(100); // Seems to be a timing issue here, sometimes
      System.out.println("This should cause an error -- missing class");
      IPC.IPC_publishData("h", ds1);
      IPC.IPC_listenWait(500);
      IPC.IPC_msgClass("h", struct1.class);
      IPC.IPC_publishData("h", ds1);
      IPC.IPC_listenWait(500);
      System.out.print("printData: ");
      IPC.IPC_printData(IPC.IPC_msgFormatter("h"), System.out, ds1);
      IPC.IPC_disconnect();
    }

    private static class msgHandler4 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object data,
			  Object clientData) {
	struct1 ds = (struct1)data;
	System.out.println("msgHandler4: "+IPC.IPC_msgInstanceName(msgInstance)+
			   " {"+ds.i+", {"+ds.a1.str+","+ds.a1.d+"}} "+
			   clientData);
	IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
	try { IPC.IPC_marshall(IPC.IPC_msgFormatter("g"), 9876, vc);
	} catch (Exception e) { 
	  System.err.println("ERROR:"+e.getMessage()); e.printStackTrace(); }
	IPC.IPC_respondVC(msgInstance, "g", vc);
      }
    }

    private static class msgHandler5 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object data,
			  Object clientData) {
	struct1 ds = (struct1)data;
	System.out.println("msgHandler5: "+IPC.IPC_msgInstanceName(msgInstance)
			   +" {"+ds.i+", {"+ds.a1.str+","+ds.a1.d+"}} "
			   +clientData);
	if (((struct1)data).i != 0)
	  IPC.IPC_respondData(msgInstance, "g", 9876);
	else
	  IPC.IPC_respondData(msgInstance, "k", data);
      }
    }

    private static class msgHandler6 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object data,
			  Object clientData) {
	System.out.println("msgHandler6: "+IPC.IPC_msgInstanceName(msgInstance)
			   +" "+data+" "+clientData);
      }
    }

    public static void test17 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_defineMsg("f", IPC.IPC_VARIABLE_LENGTH, "{int, {string, double}}");
      IPC.IPC_defineMsg("g", IPC.IPC_VARIABLE_LENGTH, "int");
      IPC.IPC_defineMsg("h", IPC.IPC_VARIABLE_LENGTH, null);
      IPC.IPC_defineMsg("k", IPC.IPC_VARIABLE_LENGTH, "{int, {string, double}}");
      IPC.IPC_msgClass("f", struct1.class);
      IPC.IPC_msgClass("g", int.class);
      IPC.IPC_msgClass("k", struct1.class);
      IPC.IPC_subscribeData("f", new msgHandler4(), null);
      IPC.IPC_subscribeData("g", new msgHandler2(), null);
      IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
      struct1 ds = new struct1(666, new struct2("hello", 3.14159));
      IPC.IPC_marshall(IPC.IPC_msgFormatter("f"), ds, vc);
      SWIGTYPE_p_void byteArray = 
	  IPC.IPC_queryResponseVC("f", vc, IPC.IPC_WAIT_FOREVER);
      Object obj = IPC.IPC_unmarshallData(IPC.IPC_msgFormatter("g"),
					  byteArray, int.class);
      IPC.IPC_freeByteArray(byteArray);
      System.out.println("Reply from query: "+ obj);

      IPC.IPC_publishData("g", 4321);
      IPC.IPC_listenWait(500);

      IPC.IPC_unsubscribe("f", msgHandler4.class);
      IPC.IPC_subscribeData("f", new msgHandler5(), null);
      obj = IPC.IPC_queryResponseData("f", ds, 5000);
      System.out.println("IPC_queryResponseData: "+obj);

      IPC.IPC_subscribeData("h", new msgHandler6(), 999);
      IPC.IPC_listenWait(100); // Seems to be a timing issue here, sometimes
      IPC.IPC_publishData("h", null);
      IPC.IPC_listenWait(500);

      ds.i = 0;
      obj = IPC.IPC_queryResponseData("f", ds, 5000);
      System.out.println("IPC_queryResponseData: "+obj);
      IPC.IPC_disconnect();
    }

    private static class queryHandler1 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object byteArray,
			  Object clientData) {
	int obj = 0;
	try {
	  obj = (int)IPC.IPC_unmarshallData(IPC.IPC_msgFormatter("g"),
					    (SWIGTYPE_p_void)byteArray,
					    int.class);
	} catch (Exception e) { 
	  System.err.println("ERROR:"+e.getMessage()); e.printStackTrace(); }
	IPC.IPC_freeByteArray((SWIGTYPE_p_void)byteArray);
	System.out.println("queryHandler1: Reply from query: "+obj+
			   " "+clientData);
      }
    }

    private static class queryHandler2 implements IPC.HANDLER_TYPE {
      public void handle (MSG_INSTANCE msgInstance, Object data,
			  Object clientData) {
	System.out.println("queryHandler2: Reply from query: "+data+
			   " "+clientData);
      }
    }

    public static void test18 () throws Exception {
      IPC.IPC_connect("test");
      IPC.IPC_defineMsg("f", IPC.IPC_VARIABLE_LENGTH, "{int, {string, double}}");
      IPC.IPC_msgClass("f", struct1.class);
      IPC.IPC_defineMsg("g", IPC.IPC_VARIABLE_LENGTH, "int");
      IPC.IPC_msgClass("g", int.class);
      IPC.IPC_subscribeData("f", new msgHandler4(), null);
      IPC_VARCONTENT_TYPE vc = new IPC_VARCONTENT_TYPE();
      struct1 ds = new struct1(666, new struct2("hello", 3.14159));
      IPC.IPC_marshall(IPC.IPC_msgFormatter("f"), ds, vc);
      IPC.IPC_queryNotifyVC("f", vc, new queryHandler1(), 1234);
      IPC.IPC_listenWait(500);

      IPC.IPC_queryNotifyData("f", ds, new queryHandler2(), 1234);
      IPC.IPC_listenWait(500);
      IPC.IPC_disconnect();
    }

    private static class doneCount {
	public int count = 0;
	public boolean done = false;
    }

    private static class timerHnd1 implements IPC.TIMER_HANDLER_TYPE {
      public void handle (Object clientData, long currentTime, long scheduledTime) {
	doneCount _doneCount = (doneCount)clientData;
	_doneCount.count += 1;
	_doneCount.done = (_doneCount.count == 3);
	System.out.println("timerHnd1: "+_doneCount.count+" "+_doneCount.done);
      }
    }

    private static class timerHnd2 implements IPC.TIMER_HANDLER_TYPE {
      public void handle (Object clientData, long currentTime, long scheduledTime) {
	doneCount _doneCount = (doneCount)clientData;
	System.out.println("timerHnd2: ["+_doneCount.done+", "+_doneCount.count+"]");
      }
    }

    private static class timerHnd3 implements IPC.TIMER_HANDLER_TYPE {
      public void handle (Object clientData, long currentTime, long scheduledTime) {
	System.out.println("timerHnd3: "+clientData);
      }
    }

    private static class timerHnd4 implements IPC.TIMER_HANDLER_TYPE {
      public void handle (Object clientData, long currentTime, long scheduledTime) {
	System.out.println("timerHnd4: "+clientData);
      }
    }

    public static void test19 () throws Exception {
      doneCount _doneCount = new doneCount();
      IPC.IPC_connect("test");
      IPC.IPC_addTimer(1000, 3, new timerHnd1(), _doneCount);
      TIMER_REF_CONTAINER_TYPE timerRef = new TIMER_REF_CONTAINER_TYPE();
      IPC.IPC_addTimerGetRef(500, 20, new timerHnd2(), _doneCount, timerRef);
      IPC.IPC_addPeriodicTimer(1250, new timerHnd3(), 1);
      while (!_doneCount.done) IPC.IPC_listen(1000);
      IPC.IPC_removeTimer(timerHnd4.class);
      IPC.IPC_removeTimerByRef(timerRef);
      IPC.IPC_removeTimerByRef(timerRef);
      IPC.IPC_removeTimer(timerHnd3.class);
      IPC.IPC_addOneShotTimer(500, new timerHnd4(), 666);
      IPC.IPC_listen(1000);
      IPC.IPC_disconnect();
    }

  public static void main (String args[]) throws Exception {
    System.out.println("\ntest0"); test0();
    System.out.println("\ntest1"); test1();
    System.out.println("\ntest2"); test2();
    System.out.println("\ntest3"); test3();
    System.out.println("\ntest4"); test4();
    System.out.println("\ntest5"); test5();
    System.out.println("\ntest6"); test6();
    System.out.println("\ntest7"); test7();
    System.out.println("\ntest8"); test8();
    System.out.println("\ntest9"); test9();
    System.out.println("\ntest10"); test10();
    System.out.println("\ntest11"); test11();
    System.out.println("\ntest12"); test12();
    System.out.println("\ntest13"); test13();
    System.out.println("\ntest14"); test14();
    System.out.println("\ntest15"); test15();
    System.out.println("\ntest16"); test16();
    System.out.println("\ntest17"); test17();
    System.out.println("\ntest18"); test18();
    System.out.println("\ntest19"); test19();
  }
}
