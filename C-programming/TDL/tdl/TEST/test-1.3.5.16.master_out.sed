Task Control Management x.y.z (MON-DAY-YEAR)
Goal        run-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        run-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "fgoal-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                fgoal {2}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "fgoal-0"
_TDL_Serial ( Constrain=fcommand-0 , Ref=fgoal-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Command          fcommand {3}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "fcommand-0"
_TDL_Serial ( Constrain=fmonitor-0 , Ref=fcommand-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Monitor          fmonitor {4}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "fmonitor-0"
_TDL_Serial ( Constrain=fg-0 , Ref=fmonitor-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                   fg {5}:        TCM {1} --> ON HOLD         (Inactive)
Goal                fgoal {2}:  ON HOLD  --> TCM             (Sent)
  Success   run-auto,wait {1}:
fgoal:  TCM_Goal
  Success           fgoal {2}:
Command          fcommand {3}:  ON HOLD  --> TCM             (Sent)
fcommand:  TCM_Command
  Success        fcommand {3}:
Monitor          fmonitor {4}:  ON HOLD  --> TCM             (Sent)
Command      ACT-fmonitor {6}:        TCM {4} --> ON HOLD         (Inactive)
Command      ACT-fmonitor {6}:  ON HOLD  --> TCM             (Sent)
 Complete        fmonitor {4}:
fmonitor:  TCM_Monitor
  Success    ACT-fmonitor {6}:
Goal                   fg {5}:  ON HOLD  --> TCM             (Sent)
Goal                  fex {7}:        TCM {5} --> ON HOLD         (Inactive)
Goal                  fex {7}:  ON HOLD  --> TCM             (Sent)
  Failure              fg {5}:
fexh:  TCM_Goal
  Success             fex {7}:
Goal      postponedTask-auto,wait {8}:        TCM {0} --> ON HOLD         (Inactive)
Goal      postponedTask-auto,wait {8}:  ON HOLD  --> TCM             (Sent)
WARNING: [Agenda:clearQueues]  Endless loop detected: Out of pending Tasks.  No further incoming Tasks.  Infinite time delay.  Canceling clearQueues to avoid infinite block (deadlock).  (Use TCM_SetAllowInfiniteTimeouts() or TCM_SetInfiniteTimeTimeout() to disable this cancellation policy.)

main:  TDL_ARG: 5
main:  TDL_TASK_ARG: 5
