Task Control Management x.y.z (MON-DAY-YEAR)
Goal      Top-outsideTask {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal      Top-outsideTask {1}:  ON HOLD  --> TCM             (Sent)
top
Goal        B-outsideTask {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal        B-outsideTask {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=C-0 , Ref=B1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("B1")

_TDL_Serial ( Constrain=C-0 , Ref=B2 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("B2")

_TDL_DisableForTime ( Constrain=C-0 , ConstrainInterval=**UNKNOWN_INTERVAL** ,  Ref=B2 [Ref_flags: RUNNING]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=COMPLETED_STATE ,  Time= 200  (MSecs) )
Constraint:  _TDL_DisableForTime  (0x........)
 NodeToConstrainInterval = Unknown Interval
 Time= 200  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("B2")

Goal                    C {5}:        TCM {1} --> ON HOLD         (Inactive)
  Success Top-outsideTask {1}:
Goal        B-outsideTask {2}:  ON HOLD  --> TCM             (Sent)
b:
Root Node {0} [uh|ag|pg|al]
   Top-outsideTask {1} [hd|ag|pg|al]
      B-outsideTask {2} [hg|ag|pg|al]
      B-outsideTask {4} [uh|ua|up|al]
      C {5} [uh|ua|up|al]
  Success   B-outsideTask {2}:
Goal        B-outsideTask {4}:  ON HOLD  --> TCM             (Sent)
b:
Root Node {0} [uh|ag|pg|al]
   Top-outsideTask {1} [hd|ag|pg|al]
      B-outsideTask {2} [hd|ad|pd|al]
      B-outsideTask {4} [hg|ag|pg|al]
      C {5} [uh|ua|up|al]
  Success   B-outsideTask {4}:
Goal                    C {5}:  ON HOLD  --> TCM             (Sent)
c:
Root Node {0} [uh|ag|pg|al]
   Top-outsideTask {1} [hd|ag|pg|al]
      B-outsideTask {2} [hd|ad|pd|al]
      B-outsideTask {4} [hd|ad|pd|al]
      C {5} [hg|ag|pg|al]
  Success               C {5}:
