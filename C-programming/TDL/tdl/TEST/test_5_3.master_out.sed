Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_SequentialExecution ( Constrain=f1-0[. 0 0] , Ref=f2-0 [Ref_flags: NOT_ALLOCATED] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

_TDL_SequentialExpansion ( Constrain=f1-0[. 0 0] , Ref=f2-0 [Ref_flags: ALLOCATED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

_TDL_SequentialHandling ( Constrain=f1-0[. 0 0] , Ref=f2-0 [Ref_flags: ALLOCATED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {3}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f1-0[. 0 1] , Ref=f2-0 [Ref_flags: ALLOCATED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

_TDL_SequentialHandling ( Constrain=f1-0[. 0 1] , Ref=f2-0 [Ref_flags: ALLOCATED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialHandling ( Constrain=f1-0[. 1 0] , Ref=f2-0 [Ref_flags: ALLOCATED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {5}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialHandling ( Constrain=f1-0[. 1 1] , Ref=f2-0 [Ref_flags: ALLOCATED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f2-0")

Goal                   f1 {6}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialHandling ( Constrain=f3-0 , Ref=f1-0[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

_TDL_SequentialHandling ( Constrain=f3-0 , Ref=f1-0[. 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

_TDL_SequentialHandling ( Constrain=f3-0 , Ref=f1-0[. 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

_TDL_SequentialHandling ( Constrain=f3-0 , Ref=f1-0[. 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

_TDL_SequentialExpansion ( Constrain=f3-0 , Ref=f1-0[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #2")

_TDL_SequentialExpansion ( Constrain=f3-0 , Ref=f1-0[. 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #2")

_TDL_SequentialExecution ( Constrain=f3-0 , Ref=f1-0[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

Goal                   f2 {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f3 {7}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f2 {2}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
test:  f2 ( -1)
  Success              f2 {2}:
Goal                   f1 {6}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {5}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {4}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {3}:  ON HOLD  --> TCM             (Sent)
test:  f1 ( 1 , 1)
  Success              f1 {6}:
test:  f1 ( 1 , 0)
  Success              f1 {5}:
test:  f1 ( 0 , 1)
  Success              f1 {4}:
test:  f1 ( 0 , 0)
  Success              f1 {3}:
Goal                   f3 {7}:  ON HOLD  --> TCM             (Sent)
test:  f3 ( -1)
  Success              f3 {7}:
