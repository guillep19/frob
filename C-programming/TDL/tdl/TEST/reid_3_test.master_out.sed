Task Control Management x.y.z (MON-DAY-YEAR)
Goal         A1-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal         A1-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
Running A1
_TDL_TerminateAtTime ( Constrain=C-0 ,  Time= 23 : 0 : 0 . 0 )
Constraint:  _TDL_TerminateAtTime  (0x........)
 Time= 23 : 0 : 0 . 0

Goal                    C {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    B {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=B-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                    C {2}:  ON HOLD  --> TCM             (Sent)
Goal                    B {4}:  ON HOLD  --> TCM             (Sent)
Running C
  Success               C {2}:
Running B
  Success               B {4}:
  Success    A1-auto,wait {1}:
Goal         A2-auto,wait {5}:        TCM {0} --> ON HOLD         (Inactive)
Goal         A2-auto,wait {5}:  ON HOLD  --> TCM             (Sent)
Running A2
_TDL_TerminateAtTime ( Constrain=C-0 ,  Time= 23 : 0 : 0 . 0 )
Constraint:  _TDL_TerminateAtTime  (0x........)
 Time= 23 : 0 : 0 . 0

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "C-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    C {6}:        TCM {5} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "C-0"
_TDL_Serial ( Constrain=B-0 , Ref=C-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    B {7}:        TCM {5} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=B-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                    C {6}:  ON HOLD  --> TCM             (Sent)
Running C
  Success               C {6}:
Goal                    B {7}:  ON HOLD  --> TCM             (Sent)
Running B
  Success               B {7}:
  Success    A2-auto,wait {5}:
