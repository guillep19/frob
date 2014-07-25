# To run:
# gmake install in $IPC_DIR/python
# export PYTHONPATH="$IPC_DIR/python:$IPC_DIR/lib/$SYS"
#  where IPC_DIR is the location of IPC and SYS is the system type
#  (e.g., Linux-3.8)
# Run $IPC_DIR/bin/$SYS/central in a separate terminal
# export CENTRALHOST=localhost
# python
# import test; test.main()
# Follow the instructions (you need to enter input and start/stop the
#  $IPC_DIR/test/module2 program (which needs to be built first)
# You can then compare the output against test.py.output.
# Except for differences in pointer values, and occasional swaps in which
# handler fires first, they should be identical

from primFmttrs import *
from formatters import *
from _IPC import *
from IPC import *

printByteArrayP = True

def printVC (vc) :
    if (printByteArrayP) :
        printByteArray(vc.content, vc.length)

class sample0(IPCdata) :
    _fields = ('d1', 'd2')

class sample1(IPCdata) :
    _fields = ('c1', 'c2')

def test0 () :
    IPC_initialize()
    buf = createBuffer(createByteArray(10))
    ds = sample0()
    ds.d1 = 666.0
    it = DOUBLE_Trans()
    it.Encode(ds, 0, buf)
    printBuffer(buf)
    rewindBuffer(buf)
    it.Decode(ds, 1, buf)
    print ds.d2
    print it.ELength(ds, 1)
    print it.SimpleType()
    ar = [1, 2, 3]
    rewindBuffer(buf)
    it.EncodeElement(ar, 1, buf)
    printBuffer(buf)
    rewindBuffer(buf)
    it.DecodeElement(ar, 2, buf)
    print ar

def test1 () :
    IPC_initialize()
    buf = createBuffer(createByteArray(10))
    ds = sample1()
    ds.c1 = 'h'
    st = CHAR_Trans()
    st.Encode(ds, 0, buf)
    printBuffer(buf)
    rewindBuffer(buf)
    st.Decode(ds, 1, buf)
    print ds.c2
    print st.ELength(ds, 1)
    print st.SimpleType()
    ar = ['e', 'm', 'c']
    rewindBuffer(buf)
    st.EncodeElement(ar, 1, buf)
    printBuffer(buf)
    rewindBuffer(buf)
    st.DecodeElement(ar, 2, buf)
    print ar

# This tests all the primitives
def test2 () :
    IPC_initialize()
    fmt1 = IPC_parseFormat("int")
    vc1 = IPC_VARCONTENT_TYPE()
    IPC_marshall(fmt1, 123, vc1)
    printVC(vc1)
    print IPC_unmarshallData(fmt1, vc1.content, int)

    vc3 = IPC_VARCONTENT_TYPE()
    fmt3 = IPC_parseFormat("boolean")
    IPC_marshall(fmt3, True, vc3)
    printVC(vc3)
    print IPC_unmarshallData(fmt3, vc3.content, bool)

    vc4 = IPC_VARCONTENT_TYPE()
    fmt4 = IPC_parseFormat("float")
    IPC_marshall(fmt4, 55.0, vc4)
    printVC(vc4)
    print IPC_unmarshallData(fmt4, vc4.content, float)

    vc2 = IPC_VARCONTENT_TYPE()
    fmt2 = IPC_parseFormat("double")
    IPC_marshall(fmt2, 666.0, vc2)
    printVC(vc2)
    print IPC_unmarshallData(fmt2, vc2.content, float)

    vc5 = IPC_VARCONTENT_TYPE()
    fmt5 = IPC_parseFormat("byte")
    IPC_marshall(fmt5, 0XA, vc5)
    printVC(vc5)
    print IPC_unmarshallData(fmt5, vc5.content, int)

    vc5 = IPC_VARCONTENT_TYPE()
    fmt5 = IPC_parseFormat("ubyte")
    IPC_marshall(fmt5, 0XFA, vc5)
    printVC(vc5)
    print IPC_unmarshallData(fmt5, vc5.content, int)

    vc6 = IPC_VARCONTENT_TYPE()
    fmt6 = IPC_parseFormat("string")
    IPC_marshall(fmt6, "hello", vc6)
    printVC(vc6)
    print IPC_unmarshallData(fmt6, vc6.content, str)

    vc6 = IPC_VARCONTENT_TYPE()
    fmt6 = IPC_parseFormat("string")
    IPC_marshall(fmt6, "", vc6)
    printVC(vc6)
    print IPC_unmarshallData(fmt6, vc6.content, str)

#    vc6 = IPC_VARCONTENT_TYPE()
#    fmt6 = IPC_parseFormat("string")
#    IPC_marshall(fmt6, None, vc6)
#    printVC(vc6)
#    print IPC_unmarshallData(fmt6, vc6.content, str)

    vc7 = IPC_VARCONTENT_TYPE()
    fmt7 = IPC_parseFormat("char")
    IPC_marshall(fmt7, 'c', vc7)
    printVC(vc7)
    print IPC_unmarshallData(fmt7, vc7.content, str)

    vc8 = IPC_VARCONTENT_TYPE()
    fmt8 = IPC_parseFormat("short")
    IPC_marshall(fmt8, 666, vc8)
    printVC(vc8)
    print IPC_unmarshallData(fmt8, vc8.content, int)

    vc9 = IPC_VARCONTENT_TYPE()
    fmt9 = IPC_parseFormat("long")
    IPC_marshall(fmt9, 0X7FFFFFFF, vc9)
    printVC(vc9)
    print IPC_unmarshallData(fmt9, vc9.content, long)

class struct1(IPCdata) :
    _fields = ('i', ('a1', 'struct2'))

class struct2(IPCdata) :
    _fields = ('str', 'd')

# test structures
def test3 () :
    IPC_initialize()
    fmt1 = IPC_parseFormat("{int, {string, double}}")
    vc1 = IPC_VARCONTENT_TYPE()
    ds = IPCdata()
    ds._f0 = 666
    ds._f1 = IPCdata();
    ds._f1._f0 = "hello"; ds._f1._f1 = 3.14159
    IPC_marshall(fmt1, ds, vc1)
    printVC(vc1)
    print IPC_unmarshallData(fmt1, vc1.content, struct1)

    ds1 = struct1()
    ds2 = struct2()
    ds1.i = 1234; ds1.a1 = ds2
    ds2.str = "eat more spam"; ds2.d = 9.87654321
    IPC_marshall(fmt1, ds1, vc1)
    printVC(vc1)
    ds1a = struct1()
    ret = IPC_unmarshall(fmt1, vc1.content, ds1a)
    print (ds1a, ret, ds1 == ds1a, ds2 == ds1a.a1)

    # Should raise an error
    try: IPC_unmarshall(fmt1, vc1.content, ds2)
    except Exception, e : print(e)

# test fixed arrays
def test4 () :
    IPC_initialize()
    fmt1 = IPC_parseFormat("[int :5]")
    vc1 = IPC_VARCONTENT_TYPE()
    ds1 = range(10,15)
    IPC_marshall(fmt1, ds1, vc1)
    printVC(vc1)
    ds1a = [None]*5
    IPC_unmarshall(fmt1, vc1.content, ds1a)
    print ds1a

    fmt2 = IPC_parseFormat("[{string, double} :5]")
    vc2 = IPC_VARCONTENT_TYPE()
    ds2 = [None]*5
    foo = ('eat', 'more', 'spam', 'for', 'life')
    for i in range(0,5) :
        ds2[i] = struct2()
        ds2[i].str = foo[i]
        ds2[i].d = float(pow(i,3))
    IPC_marshall(fmt2, ds2, vc2)
    printVC(vc2)
    print IPC_unmarshallData(fmt2, vc2.content, struct2)

    fmt3 = IPC_parseFormat("[int : 3, 4]")
    vc3 = IPC_VARCONTENT_TYPE()
    ds3 = [None]*3; ds3a = [None]*3
    for i in range(0, 3) :
        ds3[i] = range(pow(i+1,2), pow(i+1,2)+4)
        ds3a[i] = [None]*4
    IPC_marshall(fmt3, ds3, vc3)
    printVC(vc3)
    IPC_unmarshall(fmt3, vc3.content, ds3a)
    print ds3a

    fmt4 = IPC_parseFormat("[double : 3, 4]")
    vc4 = IPC_VARCONTENT_TYPE()
    ds4 = [None]*3
    for i in range(0, 3) :
      ds4[i] = [None]*4
      for j in range(0, 4) : ds4[i][j] = (float)(pow(i+1,2)+pow(j+1,2)+4)
    IPC_marshall(fmt4, ds4, vc4)
    printVC(vc4)
    print IPC_unmarshallData(fmt4, vc4.content, int)

class struct4(IPCdata) :
    _fields = ('num', 'ar')

class struct5(IPCdata) :
    _fields = (('ar', 'struct2'), 'num')

class struct6(IPCdata) :
    _fields = ('dim1', 'dim2', 'ar')

# test variable arrays
def test5 () :
    IPC_initialize()
    fmt1 = IPC_parseFormat("{int, <int :1>}")
    vc1 = IPC_VARCONTENT_TYPE()
    ds1 = struct4(); ds1a = struct4()
    ds1.num = 5; ds1.ar = range(111,116)
    IPC_marshall(fmt1, ds1, vc1)
    printVC(vc1)
    IPC_unmarshall(fmt1, vc1.content, ds1a)
    print ds1a

    fmt2 = IPC_parseFormat("{<{string, double} :2>, int}")
    vc2 = IPC_VARCONTENT_TYPE()
    ds2 = struct5()
    ds2.ar = [None]*5; ds2.num = 5
    foo = ('eat', 'more', 'spam', 'for', 'life')
    for i in range(0,5) :
        ds2.ar[i] = struct2()
        ds2.ar[i].str = foo[i]
        ds2.ar[i].d = float(pow(i,3))
    IPC_marshall(fmt2, ds2, vc2)
    printVC(vc2)
    print IPC_unmarshallData(fmt2, vc2.content, struct5)

    fmt3 = IPC_parseFormat("{int, int, <int : 1, 2>}")
    vc3 = IPC_VARCONTENT_TYPE()
    ds3 = struct6()
    ds3.dim1 = 3; ds3.dim2 = 4; ds3.ar = [None]*ds3.dim1
    for i in range(0, 3) : ds3.ar[i] = range(pow(i+1,2), pow(i+1,2)+ds3.dim2)
    IPC_marshall(fmt3, ds3, vc3)
    printVC(vc3)
    ds3a = struct6();
    IPC_unmarshall(fmt3, vc3.content, ds3a)
    print ds3a
    print IPC_unmarshallData(fmt3, vc3.content, struct6)

# Stand-in for enumerated type
WaitVal = 0; SendVal = 1; ReceiveVal = 2; ListenVal = 3
class struct7(IPCdata) :
    _fields = ('i1', 'status')

# test enums
def test6 () :
    IPC_initialize()
    fmt1 = IPC_parseFormat("{int, {enum WaitVal, SendVal, ReceiveVal, ListenVal}}")
    vc1 = IPC_VARCONTENT_TYPE()
    ds1 = struct7(); ds1a = struct7()
    ds1.i1 = 42; ds1.status = ReceiveVal
    IPC_marshall(fmt1, ds1, vc1)
    printVC(vc1)
    IPC_unmarshall(fmt1, vc1.content, ds1a)
    print ds1a

    fmt2 = IPC_parseFormat("[{enum WaitVal, SendVal, ReceiveVal, ListenVal}: 3]")
    vc2 = IPC_VARCONTENT_TYPE()
    ds2 = [None]*3; ds2[0] = SendVal; ds2[1] = ListenVal; ds2[2] = WaitVal; 
    IPC_marshall(fmt2, ds2, vc2)
    printVC(vc2)
    print IPC_unmarshallData(fmt2, vc2.content, struct7)

    fmt3 = IPC_parseFormat("{int, {enum : 4}}")
    vc3 = IPC_VARCONTENT_TYPE()
    ds3 = struct7(); ds3.i1 = 42; ds3.status = ReceiveVal
    IPC_marshall(fmt3, ds3, vc3)
    printVC(vc3)
    print IPC_unmarshallData(fmt3, vc3.content, struct7)

# test pointers
def test7 () :
    IPC_initialize()
    fmt1 = IPC_parseFormat("{int, {string, double}}")
    vc1 = IPC_VARCONTENT_TYPE()
    ds1 = struct1(); ds1a = struct1()
    ds1.i = 666; ds1.a1 = struct2();
    ds1.a1.str = "hello"; ds1.a1.d = 3.14159
    IPC_marshall(fmt1, ds1, vc1)
    printVC(vc1)
    IPC_unmarshall(fmt1, vc1.content, ds1a)
    print ds1a

    fmt2 = IPC_parseFormat("{*int, *{string, double}}")
    vc2 = IPC_VARCONTENT_TYPE()
    ds2 = struct1(); ds2a = struct1()
    ds2.i = 666; ds2.a1 = struct2();
    ds2.a1.str = "hello"; ds2.a1.d = 3.14159
    IPC_marshall(fmt2, ds2, vc2)
    printVC(vc2)
    IPC_unmarshall(fmt2, vc2.content, ds2a)
    print ds2a

    fmt3 = fmt2
    vc3 = IPC_VARCONTENT_TYPE()
    ds3 = struct1()
    ds3.i = 666; ds3.a1 = None
    IPC_marshall(fmt3, ds3, vc3)
    printVC(vc3)
    print IPC_unmarshallData(fmt3, vc3.content, struct1)

# test named formatters
# Need to be connected to IPC central to test this...
def test8 () :
    IPC_initialize()
    print IPC_parseFormat("")
    print IPC_parseFormat(None)

def msgHandler1 (msgInstance, data, clientData) :
  print 'msgHandler1:', IPC_msgInstanceName(msgInstance), data, clientData

def msgHandler2 (msgInstance, data, clientData) :
  print 'msgHandler2:', IPC_msgInstanceName(msgInstance), data, clientData, \
        IPC_dataLength(msgInstance)

def test9 () :
  IPC_connect("test")
  IPC_defineMsg("f", IPC_VARIABLE_LENGTH, "int")
  IPC_msgClass("f", int)
  IPC_subscribeData("f", msgHandler1, 1)
  IPC_listenWait(100) # Seems to be a timing issue here, sometimes
  IPC_publishData("f", 42)
  IPC_listenWait(500)
  print

  IPC_subscribeData("f", msgHandler2, 3)
  IPC_publishData("f", 666)
  IPC_listenWait(500)
  print

  IPC_subscribeData("f", msgHandler1, 2)
  print "Num handlers:", IPC_numHandlers("f")
  IPC_publishData("f", 1234)
  IPC_listenWait(500)
  print

  IPC_unsubscribe("f", msgHandler2)
  print "Num handlers:", IPC_numHandlers("f")
  vc = IPC_VARCONTENT_TYPE()
  IPC_marshall(IPC_msgFormatter("f"), 4321, vc)
  IPC_publishVC("f", vc)
  IPC_listenWait(500)
  IPC_disconnect()

exit = False

def stdinHnd (fd, clientData) :
  global exit
  msg = sys.stdin.readline()
  if (msg[0] in ('?', 'h')) :
    print "h: help"
    print "q: quit"
    print "u: stop listening"
  elif (msg[0] == 'q') :
    print "quit"
    exit = True
  elif (msg[0] == 'u') :
    print "Silent"
    IPC_unsubscribeFD(fd, stdinHnd)
  else :
    print "Unhandled input:", msg

def test10 () :
  global exit
  IPC_connect("test")
  IPC_subscribeFD(sys.stdin.fileno(), stdinHnd)
  exit = False
  print "Please type -- either 'h' or 'q', end with a 'q'"
  while (not exit) : IPC_listen(1000)
  IPC_disconnect()

def test11 () :
  IPC_connect("test")
  IPC_perror("Test")
  IPC_setCapacity(3)
  IPC_defineMsg("f", IPC_VARIABLE_LENGTH, "int")
  IPC_setMsgQueueLength("f", 1)
  IPC_setMsgPriority("f", 2)
  IPC_setVerbosity(IPC_Print_Errors)
  IPC_disconnect()
  IPC_defineMsg("h", IPC_VARIABLE_LENGTH, "int")
  print "HERE"
  IPC_disconnect()

def test12 () :
  IPC_connect("test")
  IPC_defineMsg("f", IPC_VARIABLE_LENGTH, "int")
  IPC_defineMsg("h", IPC_VARIABLE_LENGTH, "int")
  print 'Here1'
  IPC_subscribeData("f", msgHandler1, 1)
  print 'Here2'
  IPC_subscribeData("f", msgHandler1, 2)
  print 'Here3'
  IPC_subscribeData("g", msgHandler1, 1)
  print 'Here4'
  IPC_disconnect()

def msgHandler3 (msgInstance, byteArray, clientData) :
  (data, retVal) = IPC_unmarshallData(IPC_msgFormatter("g"), byteArray, int)

  print 'msgHandler3:', IPC_msgInstanceName(msgInstance), data, clientData, \
        IPC_dataLength(msgInstance)
  IPC_freeByteArray(byteArray)

def test13 () :
  IPC_connect("test")
  IPC_defineMsg("f", 4, None)
  IPC_subscribe("f", msgHandler3) # optional client data
  IPC_defineMsg("g", IPC_VARIABLE_LENGTH, "int")
  IPC_listenWait(100); # Seems to be a timing issue here, sometimes
  vc = IPC_VARCONTENT_TYPE()
  IPC_marshall(IPC_parseFormat("int"), 1234, vc)
  IPC_publish("f", vc.length, vc.content)
  IPC_listenWait(500)
  IPC_publishVC("f", vc)
  IPC_listenWait(500)
  IPC_publishFixed("f", vc.content)
  IPC_listenWait(500)
  IPC_disconnect()

connected = False
disconnected = False

def connectHandler1 (moduleName, clientData) :
  global connected
  connected = True
  print "connectHandler1:", moduleName, clientData

def connectHandler2 (moduleName, clientData) :
  print "connectHandler2:", moduleName, clientData

def disconnectHandler1 (moduleName, clientData) :
  global disconnected
  disconnected = True
  print "disconnectHandler1:", moduleName, clientData

def disconnectHandler2 (moduleName, clientData) :
  print "disconnectHandler2:", moduleName, clientData

def test14 () :
  global connected, disconnected
  IPC_connect("test")
  IPC_subscribeConnect(connectHandler1)
  IPC_subscribeDisconnect(disconnectHandler1)  
  connected = False; disconnected = False
  print "Please start, then quit from, test/module2"
  while (not connected or not disconnected) : IPC_listen(1000)
  print "HERE1"

  IPC_subscribeConnect(connectHandler1, 1)
  IPC_subscribeConnect(connectHandler2)
  IPC_subscribeDisconnect(disconnectHandler1, 1)
  IPC_subscribeDisconnect(disconnectHandler2)  
  connected = False; disconnected = False
  print "Please start, then quit from, test/module2"
  while (not connected or not disconnected) : IPC_listen(1000)
  print "HERE2"

  IPC_unsubscribeConnect(connectHandler2)
  IPC_unsubscribeDisconnect(disconnectHandler2)  
  connected = False; disconnected = False
  print "Please start, then quit from, test/module2"
  while (not connected or not disconnected) : IPC_listen(1000)
  print "HERE3"

  IPC_unsubscribeConnect(connectHandler1)
  connected = True; disconnected = False
  print "Please start, then quit from, test/module2"
  print "  (shouldn't do anything on connection, this time)"
  while (not connected or not disconnected) : IPC_listen(1000)
  print "HERE4"
  IPC_disconnect()

changed = False

def changeHandler1 (msgName, numHandlers, clientData) :
  global changed
  changed = True
  print "changeHandler1:", msgName, numHandlers, clientData

def changeHandler2 (msgName, numHandlers, clientData) :
  print "changeHandler2:", msgName, numHandlers, clientData

def test15 () :
  global disconnected, changed
  IPC_connect("test")
  IPC_defineMsg("message1", IPC_VARIABLE_LENGTH, "int")
  IPC_subscribeHandlerChange("message1", changeHandler1)
  changed = False
  print "Please start test/module2"
  while (not changed) : IPC_listen(1000)
  print "HERE1"

  IPC_subscribeHandlerChange("message1", changeHandler1, 1)
  IPC_subscribeHandlerChange("message1", changeHandler2)
  changed = False
  print "Please quit from test/module2"
  while (not changed) : IPC_listen(1000)
  print "HERE2"

  IPC_unsubscribeHandlerChange("message1", changeHandler2)
  changed = False
  print "Please start test/module2"
  while (not changed) : IPC_listen(1000)
  print "HERE3"

  IPC_subscribeDisconnect(disconnectHandler1, 123)
  IPC_unsubscribeHandlerChange("message1", changeHandler1)
  disconnected = False
  print "Please quit from test/module2"
  print "  (shouldn't call the change handler, this time)"
  while (not disconnected) : IPC_listen(1000)
  print "HERE4"
  IPC_disconnect()

def test16 () :
  IPC_connect("test")
  IPC_defineMsg("h", IPC_VARIABLE_LENGTH, "{int, fooType}")
  IPC_defineFormat("fooType", "{string, double}")
  IPC_subscribeData("h", msgHandler2)
  ds1 = struct1()
  ds1.i = 666; ds1.a1 = struct2();
  ds1.a1.str = "hello"; ds1.a1.d = 3.14159
  IPC_setVerbosity(IPC_Print_Errors)
  print "Do formats match? {int, {string, double}}: %d" % \
        IPC_checkMsgFormats("h", "{int, {string, double}}")
  print "Do formats match? {int, fooType}: %d" % \
        IPC_checkMsgFormats("h", "{int, fooType}")
  print "Do formats match? {int, double}: %d" % \
        IPC_checkMsgFormats("h", "{int, double}")
  IPC_listenWait(100) # Seems to be a timing issue here, sometimes
  IPC_publishData("h", ds1)
  IPC_listenWait(500)
  IPC_msgClass("h", struct1)
  IPC_publishData("h", ds1)
  IPC_listenWait(500)
  print "printData: ",
  sys.stdout.flush()
  IPC_printData(IPC_msgFormatter("h"), sys.stdout, ds1)
  IPC_disconnect()

def msgHandler4 (msgInstance, data, clientData) :
  print 'msgHandler4:', IPC_msgInstanceName(msgInstance), data, clientData
  vc = IPC_VARCONTENT_TYPE()
  IPC_marshall(IPC_msgFormatter("g"), 9876, vc)
  IPC_respondVC(msgInstance, "g", vc)

def msgHandler5 (msgInstance, data, clientData) :
  print 'msgHandler5:', IPC_msgInstanceName(msgInstance), data, clientData
  if (data.i != 0) :
    IPC_respondData(msgInstance, "g", 9876)
  else :
    IPC_respondData(msgInstance, "k", data)

def msgHandler6 (msgInstance, data, clientData) :
  print 'msgHandler6:', IPC_msgInstanceName(msgInstance), data, clientData

def test17 () :
  IPC_connect("test")
  IPC_defineMsg("f", IPC_VARIABLE_LENGTH, "{int, {string, double}}")
  IPC_defineMsg("g", IPC_VARIABLE_LENGTH, "int")
  IPC_defineMsg("h", IPC_VARIABLE_LENGTH, None)
  IPC_defineMsg("k", IPC_VARIABLE_LENGTH, "{int, {string, double}}")
  IPC_msgClass("f", struct1)
  IPC_msgClass("g", int)
  IPC_msgClass("k", struct1)
  IPC_subscribeData("f", msgHandler4, None)
  IPC_subscribeData("g", msgHandler2, None)
  vc = IPC_VARCONTENT_TYPE()
  ds = struct1()
  ds.i = 666; ds.a1 = struct2();
  ds.a1.str = "hello"; ds.a1.d = 3.14159
  IPC_marshall(IPC_msgFormatter("f"), ds, vc)
  (byteArray, ret) = IPC_queryResponseVC("f", vc, IPC_WAIT_FOREVER)
  (obj, ret) = IPC_unmarshallData(IPC_msgFormatter("g"), byteArray, int)
  IPC_freeByteArray(byteArray)
  print "Reply from query:", obj

  IPC_publishData("g", 4321)
  IPC_listenWait(500)

  IPC_unsubscribe("f", msgHandler4)
  IPC_subscribeData("f", msgHandler5, None)
  obj = IPC_queryResponseData("f", ds, 5000)
  print "IPC_queryResponseData:", obj

  IPC_subscribeData("h", msgHandler6, 999)
  IPC_listenWait(100) # Seems to be a timing issue here, sometimes
  IPC_publishData("h", None)
  IPC_listenWait(500)

  ds.i = 0
  obj = IPC_queryResponseData("f", ds, 5000)
  print "IPC_queryResponseData:", obj
  IPC_disconnect()

def queryHandler1 (msgInstance, byteArray, clientData) :
  (obj, ret) = IPC_unmarshallData(IPC_msgFormatter("g"), byteArray, int)
  IPC_freeByteArray(byteArray)
  print "queryHandler1: Reply from query:", obj, clientData

def queryHandler2 (msgInstance, data, clientData) :
  print "queryHandler2: Reply from query:", data, clientData

def test18 () :
  IPC_connect("test")
  IPC_defineMsg("f", IPC_VARIABLE_LENGTH, "{int, {string, double}}")
  IPC_msgClass("f", struct1)
  IPC_defineMsg("g", IPC_VARIABLE_LENGTH, "int")
  IPC_msgClass("g", int)
  IPC_subscribeData("f", msgHandler4, None)
  vc = IPC_VARCONTENT_TYPE()
  ds = struct1()
  ds.i = 666; ds.a1 = struct2();
  ds.a1.str = "hello"; ds.a1.d = 3.14159
  IPC_marshall(IPC_msgFormatter("f"), ds, vc)
  IPC_queryNotifyVC("f", vc, queryHandler1, 1234)
  IPC_listenWait(500)

  IPC_queryNotifyData("f", ds, queryHandler2, 1234)
  IPC_listenWait(500)
  IPC_disconnect()

def timerHnd1 (doneCount, currentTime, scheduledTime) :
  doneCount[1] = doneCount[1] + 1
  doneCount[0] = (doneCount[1] == 3)
  print "timerHnd1:", doneCount[1], doneCount[0]

def timerHnd2 (clientData, currentTime, scheduledTime) :
  print "timerHnd2:", clientData

def timerHnd3 (clientData, currentTime, scheduledTime) :
  print "timerHnd3:", clientData

def timerHnd4 (clientData, currentTime, scheduledTime) :
  print "timerHnd4:", clientData

def test19 () :
  doneCount = [False, 0]
  IPC_connect("test")
  IPC_addTimer(1000, 3, timerHnd1, doneCount)
  timerRef = TIMER_REF_CONTAINER_TYPE()
  IPC_addTimerGetRef(500, 20, timerHnd2, doneCount, timerRef)
  IPC_addPeriodicTimer(1250, timerHnd3, 1)
  while (not doneCount[0]) : IPC_listen(1000)
  IPC_removeTimer(timerHnd4)
  IPC_removeTimerByRef(timerRef)
  IPC_removeTimerByRef(timerRef)
  IPC_removeTimer(timerHnd3)
  IPC_addOneShotTimer(500, timerHnd4, 666)
  IPC_listen(1000)
  IPC_disconnect()

def main () :
  print "\ntest0"; test0()
  print "\ntest1"; test1()
  print "\ntest2"; test2()
  print "\ntest3"; test3()
  print "\ntest4"; test4()
  print "\ntest5"; test5()
  print "\ntest6"; test6()
  print "\ntest7"; test7()
  print "\ntest8"; test8()
  print "\ntest9"; test9()
  print "\ntest10"; test10()
  print "\ntest11"; test11()
  print "\ntest12"; test12()
  print "\ntest13"; test13()
  print "\ntest14"; test14()
  print "\ntest15"; test15()
  print "\ntest16"; test16()
  print "\ntest17"; test17()
  print "\ntest18"; test18()
  print "\ntest19"; test19()
