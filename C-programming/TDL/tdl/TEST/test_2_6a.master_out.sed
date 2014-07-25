Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_TerminateAtTime ( Constrain=f1-0 ,  Time= 22 : 50 : 0 . 0 )
Constraint:  _TDL_TerminateAtTime  (0x........)
 Time= 22 : 50 : 0 . 0

Goal                   f1 {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f1-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f1 {2}:  ON HOLD  --> TCM             (Sent)
test-f1: 0- terminate at 22:50:0.0
  Success              f1 {2}:
  Success   foo-auto,wait {1}:
