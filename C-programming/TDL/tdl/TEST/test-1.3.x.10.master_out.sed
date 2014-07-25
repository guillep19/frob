Task Control Management x.y.z (MON-DAY-YEAR)
Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
test:  16384
Goal                  foo {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_DisableForTime ( Constrain=bar-0 , ConstrainInterval=**UNKNOWN_INTERVAL** ,  Ref=foo-0 [Ref_flags: RUNNING]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=COMPLETED_STATE ,  Time= 1000  (MSecs) )
Constraint:  _TDL_DisableForTime  (0x........)
 NodeToConstrainInterval = Unknown Interval
 Time= 1000  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("foo-0")

Goal                  bar {3}:        TCM {1} --> ON HOLD         (Inactive)
Goal                  foo {2}:  ON HOLD  --> TCM             (Sent)
  Success  test-auto,wait {1}:
foo [NOT THREADED]:  16384
  Success             foo {2}:
Goal                  bar {3}:  ON HOLD  --> TCM             (Sent)
bar [THREADED]:  16386
  Success             bar {3}:
