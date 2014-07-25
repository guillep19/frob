Task Control Management x.y.z (MON-DAY-YEAR)
Goal        A-outsideTask {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        A-outsideTask {1}:  ON HOLD  --> TCM             (Sent)
Doing A
Goal                    B {2}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "B-0"
_TDL_Serial ( Constrain=C-0 , Ref=B-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    C {3}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "C-0"
_TDL_SequentialExecution ( Constrain=D-0 , Ref=C-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Command                 D {4}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    B {2}:  ON HOLD  --> TCM             (Sent)
  Success   A-outsideTask {1}:
Doing B
Goal                    C {5}:        TCM {2} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=F-0 , Ref=C-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("C-0")

Goal                    F {6}:        TCM {2} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=G-0 , Ref=C-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("C-0")

Goal                    G {7}:        TCM {2} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=D-0 , Ref=F-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("F-0")

_TDL_SequentialExecution ( Constrain=D-0 , Ref=G-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("G-0")

Command                 D {8}:        TCM {2} --> ON HOLD         (Inactive)
Goal                    C {5}:  ON HOLD  --> TCM             (Sent)
Goal                    F {6}:  ON HOLD  --> TCM             (Sent)
Goal                    G {7}:  ON HOLD  --> TCM             (Sent)
  Success               B {2}:
Command       E-auto,wait {9}:        TCM {5} --> ON HOLD         (Inactive)
Command       E-auto,wait {9}:  ON HOLD  --> TCM             (Sent)
  Success               F {6}:
  Success               G {7}:
  Success     E-auto,wait {9}:
  Success               C {5}:
Command                 D {8}:  ON HOLD  --> TCM             (Sent)
  Success               D {8}:
Goal                    C {3}:  ON HOLD  --> TCM             (Sent)
Command       E-auto,wait {10}:        TCM {3} --> ON HOLD         (Inactive)
Command       E-auto,wait {10}:  ON HOLD  --> TCM             (Sent)
  Success     E-auto,wait {10}:
  Success               C {3}:
Command                 D {4}:  ON HOLD  --> TCM             (Sent)
  Success               D {4}:
Doing C
Doing F
Doing G
Doing E
Doing D
Doing C
Doing E
Doing D
