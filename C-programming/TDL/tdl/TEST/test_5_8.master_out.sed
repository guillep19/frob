Task Control Management x.y.z (MON-DAY-YEAR)
Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
[_TDL_TDLStatement:operator const TCM_Task_Tree_Ref &()]  Warning:  Assuming non-iteration statement.
Monitor                f1 {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f2 {3}:        TCM {1} --> ON HOLD         (Inactive)
Monitor                f1 {2}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {3}:  ON HOLD  --> TCM             (Sent)
  Success  test-auto,wait {1}:
Test:  Goal f2.
_TDL_ActivateImmediate ( Constrain=theMonitor )
Constraint:  _TDL_ActivateImmediate  (0x........)

_TDL_ActivateImmediate ( Constrain=aName )
Constraint:  _TDL_ActivateImmediate  (0x........)

_TDL_ActivateImmediate ( Constrain=anotherName )
Constraint:  _TDL_ActivateImmediate  (0x........)

Command            ACT-f1 {4}:        TCM {2} --> ON HOLD         (Inactive)
Command            ACT-f1 {5}:        TCM {2} --> ON HOLD         (Inactive)
Command            ACT-f1 {6}:        TCM {2} --> ON HOLD         (Inactive)
  Success              f2 {3}:
Command            ACT-f1 {4}:  ON HOLD  --> TCM             (Sent)
Test:  Monitor f1:  # 0
  Success          ACT-f1 {4}:
Command            ACT-f1 {5}:  ON HOLD  --> TCM             (Sent)
Test:  Monitor f1:  # 1
  Success          ACT-f1 {5}:
Command            ACT-f1 {6}:  ON HOLD  --> TCM             (Sent)
Test:  Monitor f1:  # 2
 Complete              f1 {2}:
  Success          ACT-f1 {6}:
