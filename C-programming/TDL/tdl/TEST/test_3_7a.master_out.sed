Task Control Management x.y.z (MON-DAY-YEAR)
Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_TerminateAtEvent ( Constrain=foo-0 , Ref=bar-0 [Ref_flags: NOT_ALLOCATED] , RefInterval=EXECUTION_INTERVAL , RefState=COMPLETED_STATE )
Constraint:  _TDL_TerminateAtEvent  (0x........)
 referenceInterval = Execution Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("bar-0")

Monitor               foo {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_DisableForTime ( Constrain=bar-0 , ConstrainInterval=**UNKNOWN_INTERVAL** ,  Time= 0 : 0 : 10 . 0 )
Constraint:  _TDL_DisableForTime  (0x........)
 NodeToConstrainInterval = Unknown Interval
 Time= 0 : 0 : 10 . 0
 referenceInterval = Unknown Interval
 referenceState = Unknown State
 ActualReferenceNode = (nil)   ("NULL")

Goal                  bar {3}:        TCM {1} --> ON HOLD         (Inactive)
Monitor               foo {2}:  ON HOLD  --> TCM             (Sent)
  Success  test-auto,wait {1}:
Command           ACT-foo {5}:        TCM {2} --> ON HOLD         (Inactive)
Command           ACT-foo {5}:  ON HOLD  --> TCM             (Sent)
Test-Monitor: foo [ triggers = 0, activates = 1 ]
  Success         ACT-foo {5}:
Command           ACT-foo {6}:        TCM {2} --> ON HOLD         (Inactive)
Command           ACT-foo {6}:  ON HOLD  --> TCM             (Sent)
 Complete             foo {2}:
Test-Monitor: foo [ triggers = 0, activates = 2 ]
  Success         ACT-foo {6}:
Goal                  bar {3}:  ON HOLD  --> TCM             (Sent)
Test-bar:
  Success             bar {3}:
