Task Control Management x.y.z (MON-DAY-YEAR)
Goal          A-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal          A-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "B-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    B {2}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "C-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Parallel ( Constrain=C-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "B-0"
_TDL_Serial ( Constrain=C-0 , Ref=B-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    C {3}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "C-0"
_TDL_Serial ( Constrain=D-0 , Ref=C-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Parallel ( Constrain=D-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "B-0"
_TDL_Serial ( Constrain=D-0 , Ref=B-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    D {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Parallel ( Constrain=E-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "D-0"
_TDL_Serial ( Constrain=E-0 , Ref=D-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Parallel ( Constrain=E-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "B-0"
_TDL_Serial ( Constrain=E-0 , Ref=B-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    E {5}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Parallel ( Constrain=F-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "D-0"
_TDL_Serial ( Constrain=F-0 , Ref=D-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Parallel ( Constrain=F-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "B-0"
_TDL_Serial ( Constrain=F-0 , Ref=B-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    F {6}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "G-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Parallel ( Constrain=G-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "B-0"
_TDL_Serial ( Constrain=G-0 , Ref=B-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    G {7}:        TCM {1} --> ON HOLD         (Inactive)
Goal          J-auto,wait {8}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    B {2}:  ON HOLD  --> TCM             (Sent)
Goal          J-auto,wait {8}:  ON HOLD  --> TCM             (Sent)
Running:  B
  Success               B {2}:
Goal                    G {7}:  ON HOLD  --> TCM             (Sent)
Goal                    C {3}:  ON HOLD  --> TCM             (Sent)
Running:  J
  Success     J-auto,wait {8}:
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "G-0"
_TDL_Serial ( Constrain=H-0 , Ref=G-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Parallel ( Constrain=H-0 )
Constraint:  _TDL_Parallel  0x........

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "B-0"
_TDL_Serial ( Constrain=H-0 , Ref=B-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    H {9}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "WithDo-1"
_TDL_Serial ( Constrain=I-0 , Ref=C-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=I-0 , Ref=D-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=I-0 , Ref=E-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=I-0 , Ref=F-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=I-0 , Ref=G-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=I-0 , Ref=H-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    I {10}:        TCM {1} --> ON HOLD         (Inactive)
  Success     A-auto,wait {1}:
Running:  G
  Success               G {7}:
Goal                    H {9}:  ON HOLD  --> TCM             (Sent)
Running:  C
  Success               C {3}:
Goal                    D {4}:  ON HOLD  --> TCM             (Sent)
Running:  H
  Success               H {9}:
Running:  D
  Success               D {4}:
Goal                    F {6}:  ON HOLD  --> TCM             (Sent)
Goal                    E {5}:  ON HOLD  --> TCM             (Sent)
Running:  F
  Success               F {6}:
Running:  E
  Success               E {5}:
Goal                    I {10}:  ON HOLD  --> TCM             (Sent)
Running:  I
  Success               I {10}:
