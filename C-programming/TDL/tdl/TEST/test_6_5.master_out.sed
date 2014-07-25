Task Control Management x.y.z (MON-DAY-YEAR)
Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
Test:  Running test().  Failing with testException(200)
Goal        testException {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal        testException {2}:  ON HOLD  --> TCM             (Sent)
  Failure  test-auto,wait {1}:
Test: (1) fooExceptionHandler:  theHandlerInt = 100,  theExceptionInt = 200,  testPersistent = 1
Goal              printme {3}:        TCM {2} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=resumeFooExceptionHandler-0 , Ref=printme-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("printme-0")

Goal      resumeFooExceptionHandler {4}:        TCM {2} --> ON HOLD         (Inactive)
Goal              printme {3}:  ON HOLD  --> TCM             (Sent)
Test:  [printme]:  foo test 1
  Success         printme {3}:
Goal      resumeFooExceptionHandler {4}:  ON HOLD  --> TCM             (Sent)
Test:   (2) resumeFooExceptionHandler ( 0x........ )
Goal      printme-auto,wait {5}:        TCM {4} --> ON HOLD         (Inactive)
Goal      printme-auto,wait {5}:  ON HOLD  --> TCM             (Sent)
Test:    [printme]:  Task resumeFooExceptionHandler: test 2
  Success printme-auto,wait {5}:
Test:     (3) Resume fooExceptionHandler ( 300 ):  theHandlerInt = 100,  theExceptionInt = 201,   testPersistent = 2
Goal              printme {6}:        TCM {2} --> ON HOLD         (Inactive)
Goal              printme {6}:  ON HOLD  --> TCM             (Sent)
  Success   testException {2}:
  Success resumeFooExceptionHandler {4}:
Test:      [printme]:  foo resume test 3
  Success         printme {6}:
