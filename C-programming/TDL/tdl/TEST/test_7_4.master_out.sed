Task Control Management x.y.z (MON-DAY-YEAR)
Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
Monitor          aMonitor {2}:        TCM {1} --> ON HOLD         (Inactive)
Monitor          aMonitor {2}:  ON HOLD  --> TCM             (Sent)
  Success  test-auto,wait {1}:
Command      ACT-aMonitor {3}:        TCM {2} --> ON HOLD         (Inactive)
Command      ACT-aMonitor {3}:  ON HOLD  --> TCM             (Sent)
Test(0):  1:  aMonitor {5, 4294967295, 0, 1000, 0)
_TDL_TerminateImmediate ( Constrain=realMonitor )
Constraint:  _TDL_TerminateImmediate  (0x........)

Will Terminate aMonitor {2} when all references to it are released
Will Terminate ACT-aMonitor {3} when all references to it are released
Terminated ACT-aMonitor {3}
Terminated aMonitor {2}
