Task Control Management x.y.z (MON-DAY-YEAR)
Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
Goal             testGoal {2}:        TCM {1} --> ON HOLD         (Inactive)
Monitor       testMonitor {3}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_ActivateImmediate ( Constrain=testMonitor-0 )
Constraint:  _TDL_ActivateImmediate  (0x........)

_TDL_ActivateImmediate ( Constrain=testMonitor-0 )
Constraint:  _TDL_ActivateImmediate  (0x........)

Test:  test
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testGoal ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testMonitor ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), test ) = TRUE
Test:  TDL::checkForTaskInstanceOf ( TCM_NodeName ( TDL_REF(THIS) ), "test-auto,wait" ) = TRUE
Test:  TDL_IS_STARTED ( testGoal ) = FALSE
Test:  TDL_IS_COMPLETED ( testGoal ) = FALSE
Test:  TDL_INSTANCE_OF ( testGoal, testGoal ) = TRUE
Test:  TDL_INSTANCE_OF ( testGoal, testMonitor ) = FALSE
Test:  TDL_INSTANCE_OF ( testGoal, test ) = FALSE
Test:  TDL_IS_STARTED ( testMonitor ) = FALSE
Test:  TDL_IS_COMPLETED ( testMonitor ) = FALSE
Test:  TDL_INSTANCE_OF ( testMonitor, testGoal ) = FALSE
Test:  TDL_INSTANCE_OF ( testMonitor, testMonitor ) = TRUE
Test:  TDL_INSTANCE_OF ( testMonitor, test ) = FALSE
Test:  test-auto,wait

Goal             testGoal {2}:  ON HOLD  --> TCM             (Sent)
Monitor       testMonitor {3}:  ON HOLD  --> TCM             (Sent)
  Success  test-auto,wait {1}:
Command   ACT-testMonitor {4}:        TCM {3} --> ON HOLD         (Inactive)
Command   ACT-testMonitor {5}:        TCM {3} --> ON HOLD         (Inactive)
Command   ACT-testMonitor {4}:  ON HOLD  --> TCM             (Sent)
Command   ACT-testMonitor {5}:  ON HOLD  --> TCM             (Sent)
 Complete     testMonitor {3}:
Test:
Test:  testGoal
Test:  TDL_TASK_IS_STARTED ( TDL_REF(THIS) ) = TRUE
Test:  TDL_TASK_IS_COMPLETED ( TDL_REF(THIS) ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testGoal ) = TRUE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testMonitor ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), test ) = FALSE

  Success        testGoal {2}:
Test:
Test:  testMonitor
Test:  TDL_TASK_IS_STARTED ( TDL_REF(THIS) ) = TRUE
Test:  TDL_TASK_IS_COMPLETED ( TDL_REF(THIS) ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testGoal ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testMonitor ) = TRUE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), test ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), ACT-testMonitor ) = TRUE
Test:  ACT-testMonitor

  Success ACT-testMonitor {4}:
Test:
Test:  testMonitor
Test:  TDL_TASK_IS_STARTED ( TDL_REF(THIS) ) = TRUE
Test:  TDL_TASK_IS_COMPLETED ( TDL_REF(THIS) ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testGoal ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), testMonitor ) = TRUE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), test ) = FALSE
Test:  TDL_TASK_INSTANCE_OF ( TDL_REF(THIS), ACT-testMonitor ) = TRUE
Test:  ACT-testMonitor

  Success ACT-testMonitor {5}:
