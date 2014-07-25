Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_SequentialHandling ( Constrain=f2-0[. 1] , Ref=f2-0[. 0] [Ref_flags: NOT_ALLOCATED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f2 {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f2 {3}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f3-0 , Ref=f2-0[. 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

_TDL_SequentialExpansion ( Constrain=f3-0 , Ref=f2-0[. 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

Goal                   f3 {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=f1-0[. 0 1] , Ref=f1-0[. 0 0] [Ref_flags: NOT_ALLOCATED] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

_TDL_Serial ( Constrain=f1-0[. 0 0] , Ref=f2-0[. 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {5}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f1-0[. 0 1] , Ref=f2-0[. 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {6}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f1-0[. 1 0] , Ref=f2-0[. 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {7}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f1-0[. 1 1] , Ref=f2-0[. 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {8}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f4-0 , Ref=f1-0[. 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

Goal                   f4 {9}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f4-1 , Ref=f1-0[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #3")

_TDL_Serial ( Constrain=f4-1 , Ref=f1-0[. 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #3")

Goal                   f4 {10}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f4-2 , Ref=f1-0[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #2")

_TDL_Serial ( Constrain=f4-2 , Ref=f1-0[. 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #2")

_TDL_Serial ( Constrain=f4-2 , Ref=f1-0[. 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #2")

_TDL_Serial ( Constrain=f4-2 , Ref=f1-0[. 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #2")

Goal                   f4 {11}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f2 {2}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
test:  f2 ( -1)
  Success              f2 {2}:
Goal                   f1 {6}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {5}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {3}:  ON HOLD  --> TCM             (Sent)
test:  f1 ( 0 , 1)
  Success              f1 {6}:
test:  f1 ( 0 , 0)
  Success              f1 {5}:
Goal                   f4 {10}:  ON HOLD  --> TCM             (Sent)
Goal                   f4 {9}:  ON HOLD  --> TCM             (Sent)
test:  f2 ( -1)
  Success              f2 {3}:
Goal                   f1 {8}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {7}:  ON HOLD  --> TCM             (Sent)
Goal                   f3 {4}:  ON HOLD  --> TCM             (Sent)
test:  f4 ( 2)
  Success              f4 {10}:
test:  f4 ( 1)
  Success              f4 {9}:
test:  f1 ( 1 , 1)
  Success              f1 {8}:
test:  f1 ( 1 , 0)
  Success              f1 {7}:
Goal                   f4 {11}:  ON HOLD  --> TCM             (Sent)
test:  f3 ( 4)
  Success              f3 {4}:
test:  f4 ( 3)
  Success              f4 {11}:
