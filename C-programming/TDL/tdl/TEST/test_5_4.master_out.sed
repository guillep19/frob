Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)

test 1

test 2
_TDL_SequentialHandling ( Constrain=f2-0 , Ref=f1-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_SequentialHandling:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f2-1 , Ref=f1-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_SequentialExpansion:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {3}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=f2-2 , Ref=f1-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_SequentialExecution:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {4}:        TCM {1} --> ON HOLD         (Inactive)

test 3
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f1-1" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                   f1 {5}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f1-1"
_TDL_Serial ( Constrain=f1-1[. 1 1] , Ref=f1-1[. 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                   f1 {6}:        TCM {1} --> ON HOLD         (Inactive)

test 4
_TDL_SequentialHandling ( Constrain=f2-3 , Ref=f1-1[. 0] [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

[_TDL_SequentialHandling:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
_TDL_SequentialHandling ( Constrain=f2-3 , Ref=f1-1[. 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

_TDL_SequentialHandling ( Constrain=f2-3 , Ref=f1-1[. 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

Goal                   f2 {7}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f2-4 , Ref=f1-1[. 0] [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("f1-1")

[_TDL_SequentialExpansion:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {8}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=f2-5 , Ref=f1-1[. 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #4")

_TDL_SequentialExecution ( Constrain=f2-5 , Ref=f1-1[. 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #4")

Goal                   f2 {9}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f2-6 , Ref=f1-1[. 0] [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-1")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {10}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f2-7 , Ref=f1-1[. 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-1")

Goal                   f2 {11}:        TCM {1} --> ON HOLD         (Inactive)

test 5
Goal                   f2 {2}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {3}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {4}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {5}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {8}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {9}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {10}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
test:  f2 ( 1)
  Success              f2 {2}:
test:  f2 ( 2)
  Success              f2 {3}:
test:  f2 ( 3)
  Success              f2 {4}:
test:  f1 ( 1 , 0)
  Success              f1 {5}:
Goal                   f1 {6}:  ON HOLD  --> TCM             (Sent)
test:  f2 ( 5)
  Success              f2 {8}:
test:  f2 ( 6)
  Success              f2 {9}:
test:  f2 ( 7)
  Success              f2 {10}:
test:  f1 ( 1 , 1)
  Success              f1 {6}:
Goal                   f2 {11}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {7}:  ON HOLD  --> TCM             (Sent)
test:  f2 ( 8)
  Success              f2 {11}:
test:  f2 ( 4)
  Success              f2 {7}:
