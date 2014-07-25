Task Control Management x.y.z (MON-DAY-YEAR)

test1:  0x........

Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_TerminateAtTime ( Constrain=f1-0 ,  Time= 23 : 59 : 59 . 0.9 )
Constraint:  _TDL_TerminateAtTime  (0x........)
 Time= 23 : 59 : 59 . 0.9

Goal                   f1 {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f1-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f1 {2}:  ON HOLD  --> TCM             (Sent)
test-f1: 0- terminate at 23:59:59.9
  Success              f1 {2}:
  Success   foo-auto,wait {1}:
