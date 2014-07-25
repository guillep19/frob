Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_DisableForTime ( Constrain=f-0 , ConstrainInterval=**UNKNOWN_INTERVAL** ,  Time= 0 : 0 : 1 . 0.2 )
Constraint:  _TDL_DisableForTime  (0x........)
 NodeToConstrainInterval = Unknown Interval
 Time= 0 : 0 : 1 . 0.2
 referenceInterval = Unknown Interval
 referenceState = Unknown State
 ActualReferenceNode = (nil)   ("NULL")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [Spawn]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_TerminateInTime ( Constrain=f-0 ,  Ref=f-0 [Ref_flags: ALLOCATED]  , RefInterval=HANDLING_INTERVAL , RefState=ENABLED_STATE ,  Time= 0 : 1 : 2 . 0.09 )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 0 : 1 : 2 . 0.09
 referenceInterval = Handling Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

Goal                    f {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                    f {2}:  ON HOLD  --> TCM             (Sent)
test-f: 0 - unconstrained
  Success               f {2}:
  Success   foo-auto,wait {1}:
