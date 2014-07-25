Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)

test 1
_TDL_Serial ( Constrain=f2-0 , Ref=f1-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {2}:        TCM {1} --> ON HOLD         (Inactive)

test 2
_TDL_Serial ( Constrain=f2-1[. 0] , Ref=f1-1[. 0] [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-1")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {3}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f2-1[. 1] , Ref=f1-1[. 1] [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-1")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f2 {4}:        TCM {1} --> ON HOLD         (Inactive)

test 3
Goal                   f1 {5}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f1 {6}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f1 {7}:        TCM {1} --> ON HOLD         (Inactive)
Goal                   f1 {8}:        TCM {1} --> ON HOLD         (Inactive)

test 4
_TDL_SequentialHandling ( Constrain=f3-0 , Ref=f1-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_SequentialHandling:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f3 {9}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f3-1 , Ref=f1-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_SequentialExpansion:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f3 {10}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=f3-2 , Ref=f1-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_SequentialExecution:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f3 {11}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialHandling ( Constrain=f4-0 , Ref=f1-1[. 0] [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

[_TDL_SequentialHandling:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
_TDL_SequentialHandling ( Constrain=f4-0 , Ref=f1-1[. 1] [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #1")

[_TDL_SequentialHandling:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f4 {12}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f4-1 , Ref=f1-1[. 0] [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("f1-1")

[_TDL_SequentialExpansion:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f4 {13}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=f4-2 , Ref=f1-1[. 0] [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f1-1")

[_TDL_SequentialExecution:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f4 {14}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialHandling ( Constrain=f5-0 , Ref=f1-2[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #3")

_TDL_SequentialHandling ( Constrain=f5-0 , Ref=f1-2[. 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #3")

_TDL_SequentialHandling ( Constrain=f5-0 , Ref=f1-2[. 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #3")

_TDL_SequentialHandling ( Constrain=f5-0 , Ref=f1-2[. 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #3")

Goal                   f5 {15}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f5-1 , Ref=f1-2[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #4")

_TDL_SequentialExpansion ( Constrain=f5-1 , Ref=f1-2[. 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("Unnamed _TDL_TreeNodeBranch #4")

Goal                   f5 {16}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=f5-2 , Ref=f1-2[. 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f1-2")

Goal                   f5 {17}:        TCM {1} --> ON HOLD         (Inactive)

test 5
Goal                   f2 {2}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {3}:  ON HOLD  --> TCM             (Sent)
Goal                   f2 {4}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {5}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {6}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {7}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {8}:  ON HOLD  --> TCM             (Sent)
Goal                   f3 {9}:  ON HOLD  --> TCM             (Sent)
Goal                   f3 {10}:  ON HOLD  --> TCM             (Sent)
Goal                   f3 {11}:  ON HOLD  --> TCM             (Sent)
Goal                   f4 {12}:  ON HOLD  --> TCM             (Sent)
Goal                   f4 {13}:  ON HOLD  --> TCM             (Sent)
Goal                   f4 {14}:  ON HOLD  --> TCM             (Sent)
Goal                   f5 {17}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
test:  f2 ( 1)
  Success              f2 {2}:
test:  f2 ( 2)
  Success              f2 {3}:
test:  f2 ( 2)
  Success              f2 {4}:
test:  f1 ( 0 , 0)
  Success              f1 {5}:
test:  f1 ( 0 , 1)
  Success              f1 {6}:
Goal                   f5 {16}:  ON HOLD  --> TCM             (Sent)
test:  f1 ( 1 , 0)
  Success              f1 {7}:
test:  f1 ( 1 , 1)
  Success              f1 {8}:
Goal                   f5 {15}:  ON HOLD  --> TCM             (Sent)
test:  f3 ( 1)
  Success              f3 {9}:
test:  f3 ( 2)
  Success              f3 {10}:
test:  f3 ( 3)
  Success              f3 {11}:
test:  f4 ( 1)
  Success              f4 {12}:
test:  f4 ( 2)
  Success              f4 {13}:
test:  f4 ( 3)
  Success              f4 {14}:
test:  f5 ( 3)
  Success              f5 {17}:
test:  f5 ( 2)
  Success              f5 {16}:
test:  f5 ( 1)
  Success              f5 {15}:
