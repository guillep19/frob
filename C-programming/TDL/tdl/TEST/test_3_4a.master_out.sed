Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [Spawn]   No previous nodes for available for "f-1" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {2}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-2 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {4}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    f {3}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    f {2}:  ON HOLD  --> TCM             (Sent)
Goal                    f {4}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
test-f: 0
  Success               f {2}:
Goal                    f {3}:  ON HOLD  --> TCM             (Sent)
test-f: 1
  Success               f {4}:
test-f: 2
  Success               f {3}:
