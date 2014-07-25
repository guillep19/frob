Task Control Management x.y.z (MON-DAY-YEAR)
Goal      run-outsideTask {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal      run-outsideTask {1}:  ON HOLD  --> TCM             (Sent)
_TDL_Serial ( Constrain=printme-1 , Ref=printme-0 [Ref_flags: NOT_ALLOCATED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("printme-0")

_TDL_Serial ( Constrain=printme-2 , Ref=printme-1 [Ref_flags: ALLOCATED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("printme-1")

_TDL_OnTermination ( Constrain=foo-0 , Ref=printme-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_OnTermination  (0x........)
 ActualReferenceNode = 0x........   ("printme-0")

_TDL_OnTermination ( Constrain=foo-0 , Ref=printme-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_OnTermination  (0x........)
 ActualReferenceNode = 0x........   ("printme-1")

_TDL_OnTermination ( Constrain=foo-0 , Ref=printme-2 [Ref_flags: RUNNING] )
Constraint:  _TDL_OnTermination  (0x........)
 ActualReferenceNode = 0x........   ("printme-2")

Goal                  foo {9}:        TCM {1} --> ON HOLD         (Inactive)
Goal                  foo {9}:  ON HOLD  --> TCM             (Sent)
  Success run-outsideTask {1}:
Test: foo
Goal      printme-onTerminate {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal      printme-onTerminate {3}:        TCM {1} --> ON HOLD         (Inactive)
Goal      printme-onTerminate {4}:        TCM {1} --> ON HOLD         (Inactive)
Will Terminate run-outsideTask {1} when all references to it are released
Goal      printme-onTerminate {2}:  ON HOLD  --> TCM             (Sent)
Goal      printme-onTerminate {3}:  ON HOLD  --> TCM             (Sent)
Goal      printme-onTerminate {4}:  ON HOLD  --> TCM             (Sent)
Goal      printme-onTerminate {10}:        TCM {9} --> ON HOLD         (Inactive)
Goal      printme-onTerminate {11}:        TCM {9} --> ON HOLD         (Inactive)
Goal      printme-onTerminate {12}:        TCM {9} --> ON HOLD         (Inactive)
Goal              printme {6}:        TCM {9} --> ON HOLD         (Inactive)
Goal              printme {7}:        TCM {9} --> ON HOLD         (Inactive)
Goal              printme {8}:        TCM {9} --> ON HOLD         (Inactive)
Will Terminate foo {9} when all references to it are released
Goal      printme-onTerminate {10}:  ON HOLD  --> TCM             (Sent)
Goal      printme-onTerminate {11}:  ON HOLD  --> TCM             (Sent)
Goal      printme-onTerminate {12}:  ON HOLD  --> TCM             (Sent)
Goal              printme {6}:  ON HOLD  --> TCM             (Sent)
Test:  [printme]:  "First  External (main) on terminate"
Will Terminate printme-onTerminate {2} when all references to it are released
Terminated printme-onTerminate {2}
Test:  [printme]:  "Second External (main) on terminate"
Will Terminate printme-onTerminate {3} when all references to it are released
Terminated printme-onTerminate {3}
Test:  [printme]:  "Third  External (main) on terminate"
Will Terminate printme-onTerminate {4} when all references to it are released
Terminated printme-onTerminate {4}
Test:  [printme]:  "First  Task level on terminate"
Will Terminate printme-onTerminate {10} when all references to it are released
Terminated printme-onTerminate {10}
Test:  [printme]:  "Second Task level on terminate"
Will Terminate printme-onTerminate {11} when all references to it are released
Terminated printme-onTerminate {11}
Test:  [printme]:  "Third  Task level on terminate"
Will Terminate printme-onTerminate {12} when all references to it are released
Terminated printme-onTerminate {12}
Test:  [printme]:  "First  Internal on terminate"
Will Terminate printme {6} when all references to it are released
Goal              printme {7}:  ON HOLD  --> TCM             (Sent)
Terminated printme {6}
Test:  [printme]:  "Second Internal on terminate"
Will Terminate printme {7} when all references to it are released
Goal              printme {8}:  ON HOLD  --> TCM             (Sent)
Terminated printme {7}
Test:  [printme]:  "Third  Internal on terminate"
Will Terminate printme {8} when all references to it are released
Terminated printme {8}
Terminated foo {9}
Terminated run-outsideTask {1}
Starting TCM_ProcessAgenda()
Starting TCM_ProcessAgenda() a second time.
Ending TCM_ProcessAgenda().
