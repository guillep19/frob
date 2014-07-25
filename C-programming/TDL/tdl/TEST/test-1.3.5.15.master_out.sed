Task Control Management x.y.z (MON-DAY-YEAR)
Goal      ParentGoal-outsideTask {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal      ParentGoal-outsideTask {1}:  ON HOLD  --> TCM             (Sent)
ParentGoal
_TDL_AddExceptionHandler ( Node=TaskA-0, handler=0x........, index=1 )
Constraint:  _TDL_AddExceptionHandler  (0x........)
 handler = 0x........ index = 1

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "TaskA-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                TaskA {3}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "TaskA-0"
_TDL_Serial ( Constrain=TaskB-0 , Ref=TaskA-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                TaskB {4}:        TCM {1} --> ON HOLD         (Inactive)
Goal                TaskA {3}:  ON HOLD  --> TCM             (Sent)
  Success ParentGoal-outsideTask {1}:
TaskA
Goal         ourException {7}:        TCM {3} --> ON HOLD         (Inactive)
Goal         ourException {7}:  ON HOLD  --> TCM             (Sent)
  Failure           TaskA {3}:
TerminateTask: TDL-Goal-TaskB_test_1_3_5_15_tdl_0
_TDL_TerminateImmediate ( Constrain=theTask )
Constraint:  _TDL_TerminateImmediate  (0x........)

Will Terminate TaskB {4} when all references to it are released
  Success    ourException {7}:
Will Terminate printme-onTerminate {5} when all references to it are released
Terminated printme-onTerminate {5}
Will Terminate printme-onTerminate {6} when all references to it are released
Terminated TaskB {4}
Will Terminate printme-onTerminate {2} when all references to it are released
Terminated printme-onTerminate {6}
Terminated printme-onTerminate {2}
