Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_Serial ( Constrain=f2-0 , Ref=f1-0[. 3 4] [Ref_flags: NOT_ALLOCATED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

Goal                   f2 {2}:        TCM {1} --> ON HOLD         (Inactive)
START for(i=0)
END for(i=0)
START for(i=1)
START for(j=0)
Goal                   f1 {4}:        TCM {1} --> ON HOLD         (Inactive)
END for(j=0)
START for(j=1)
Goal                   f1 {5}:        TCM {1} --> ON HOLD         (Inactive)
END for(j=1)
END for(i=1)
Will Terminate f1 {3} when all references to it are released
test 1
_TDL_Serial ( Constrain=f3-0 , Ref=f1-0[. 5] [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f3 {6}:        TCM {1} --> ON HOLD         (Inactive)
test 2
_TDL_Serial ( Constrain=f4-0 , Ref=f1-0[. 5] [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f4 {7}:        TCM {1} --> ON HOLD         (Inactive)
test 3
_TDL_Serial ( Constrain=f5-0 , Ref=f1-0[. 7] [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f1-0")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
Goal                   f5 {8}:        TCM {1} --> ON HOLD         (Inactive)
test 4 -- final
Goal                   f1 {4}:  ON HOLD  --> TCM             (Sent)
Goal                   f1 {5}:  ON HOLD  --> TCM             (Sent)
Goal                   f3 {6}:  ON HOLD  --> TCM             (Sent)
Goal                   f4 {7}:  ON HOLD  --> TCM             (Sent)
Goal                   f5 {8}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
Goal                   f2 {2}:  ON HOLD  --> TCM             (Sent)
Terminated f1 {3}
test:  f1 ( 1)
  Success              f1 {4}:
test:  f1 ( 1)
  Success              f1 {5}:
test:  f3 ( 1)
  Success              f3 {6}:
test:  f4 ( 2)
  Success              f4 {7}:
test:  f5 ( 3)
  Success              f5 {8}:
test:  f2 ( 0)
  Success              f2 {2}:
