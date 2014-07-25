Task Control Management x.y.z (MON-DAY-YEAR)
Goal        run-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        run-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
Goal                  bar {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateImmediate ( Constrain=bar-0 )
Constraint:  _TDL_TerminateImmediate  (0x........)

Will Terminate bar {2} when all references to it are released
Monitor   nonPollingMonitor1 {3}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_ActivateImmediate ( Constrain=nonPollingMonitor1-0 )
Constraint:  _TDL_ActivateImmediate  (0x........)

_TDL_ActivateAtEvent ( Constrain=nonPollingMonitor1-1 , Ref=bar-0 [Ref_flags: RUNNING] , RefInterval=HANDLING_INTERVAL , RefState=COMPLETED_STATE )
Constraint:  _TDL_ActivateAtEvent  (0x........)
 referenceInterval = Handling Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("bar-0")

Monitor   nonPollingMonitor1 {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_ActivateAtTime ( Constrain=nonPollingMonitor1-2 ,  Time= 0 : 0 : 1 . 0 )
Constraint:  _TDL_ActivateAtTime  (0x........)
 Time= 0 : 0 : 1 . 0

Monitor   nonPollingMonitor1 {5}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_ActivateInTime ( Constrain=nonPollingMonitor1-3 ,  Time= 0 : 0 : 2 . 0.3 )
Constraint:  _TDL_ActivateInTime  (0x........)
 Time= 0 : 0 : 2 . 0.3
 referenceInterval = Unknown Interval
 referenceState = Unknown State
 ActualReferenceNode = (nil)   ("NULL")

Monitor   nonPollingMonitor1 {7}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_ActivateInTime ( Constrain=nonPollingMonitor1-4 ,  Ref=bar-0 [Ref_flags: RUNNING]  , RefInterval=HANDLING_INTERVAL , RefState=COMPLETED_STATE ,  Time= 0 : 0 : 3 . 0.1 )
Constraint:  _TDL_ActivateInTime  (0x........)
 Time= 0 : 0 : 3 . 0.1
 referenceInterval = Handling Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("bar-0")

Monitor   nonPollingMonitor1 {8}:        TCM {1} --> ON HOLD         (Inactive)
Monitor   nonPollingMonitor2 {9}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_ActivateImmediate ( Constrain=nonPollingMonitor2-0 )
Constraint:  _TDL_ActivateImmediate  (0x........)

Goal                  bar {10}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateImmediate ( Constrain=bar-1 )
Constraint:  _TDL_TerminateImmediate  (0x........)

Will Terminate bar {10} when all references to it are released
_TDL_ActivateImmediate ( Constrain=nonPollingMonitor2-0 )
Constraint:  _TDL_ActivateImmediate  (0x........)

Test-TDL_REF(parent):  -Begin- _TDL_SpawnStatement  0x........
 AllocationFunction:  (nil)
 Base _TDL_TDLStatement class:
  -Begin- _TDL_TDLStatement  0x........
   Names:
    -Begin- _TDL_NamesList: 0x........
     count               = 3
     capacity            = 3
     allocationBlockSize = 10
     name [  0 ]         = "_TDL_HandleManager::ENCLOSING_TASK"
     name [  1 ]         = "THIS"
     name [  2 ]         = "PARENT"
    --End-- _TDL_NamesList: 0x........
   DataObjectSlist:
    -Begin- Slist: 0x........
      -Begin- _TDL_SpawnStatementData  0x........
       Associated with: 0x........  ( "_TDL_HandleManager::ENCLOSING_TASK" )
       State..........: "RUNNING"  (3)
       handleRef......: 0x........
       SpawnStatementTreeNodes:
         _TDL_SpawnStatementTreeNode:  0x........
      --End-- _TDL_SpawnStatementData  0x........
    --End-- Slist: 0x........
  ------- _TDL_TDLStatement  0x........
   Tree:
    -Begin- _TDL_SpawnStatementTreeNode  0x........
     arrayIndex..: -9002
     name........: "_TDL_HandleManager::ENCLOSING_TASK"
     parent......: (nil)
     Associated with:  
      -Begin- _TDL_SpawnStatementData  0x........
       Associated with: 0x........  ( "_TDL_HandleManager::ENCLOSING_TASK" )
       State..........: "RUNNING"  (3)
       handleRef......: 0x........
       SpawnStatementTreeNodes:
         _TDL_SpawnStatementTreeNode:  0x........
      --End-- _TDL_SpawnStatementData  0x........
    --End-- _TDL_SpawnStatementTreeNode  0x........
  --End-- _TDL_TDLStatement  0x........
--End-- _TDL_SpawnStatement  0x........

Test-TDL_REF(b2):  -Begin- _TDL_SpawnStatement  0x........
 AllocationFunction:  0x........
 Base _TDL_TDLStatement class:
  -Begin- _TDL_TDLStatement  0x........
   Names:
    -Begin- _TDL_NamesList: 0x........
     count               = 2
     capacity            = 2
     allocationBlockSize = 10
     name [  0 ]         = "bar-1"
     name [  1 ]         = "b2"
    --End-- _TDL_NamesList: 0x........
   DataObjectSlist:
    -Begin- Slist: 0x........
      -Begin- _TDL_SpawnStatementData  0x........
       Associated with: 0x........  ( "bar-1" )
       State..........: "RUNNING"  (3)
       handleRef......: 0x........
       SpawnStatementTreeNodes:
         _TDL_SpawnStatementTreeNode:  0x........
         _TDL_SpawnStatementTreeNode:  0x........
      --End-- _TDL_SpawnStatementData  0x........
    --End-- Slist: 0x........
  ------- _TDL_TDLStatement  0x........
   Tree:
    -Begin- _TDL_SpawnStatementTreeNode  0x........
     arrayIndex..: -9002
     name........: "bar-1"
     parent......: (nil)
     Associated with:  
      -Begin- _TDL_SpawnStatementData  0x........
       Associated with: 0x........  ( "bar-1" )
       State..........: "RUNNING"  (3)
       handleRef......: 0x........
       SpawnStatementTreeNodes:
         _TDL_SpawnStatementTreeNode:  0x........
         _TDL_SpawnStatementTreeNode:  0x........
      --End-- _TDL_SpawnStatementData  0x........
    --End-- _TDL_SpawnStatementTreeNode  0x........
  --End-- _TDL_TDLStatement  0x........
--End-- _TDL_SpawnStatement  0x........

Monitor   nonPollingMonitor1 {3}:  ON HOLD  --> TCM             (Sent)
Monitor   nonPollingMonitor1 {4}:  ON HOLD  --> TCM             (Sent)
Monitor   nonPollingMonitor1 {5}:  ON HOLD  --> TCM             (Sent)
Monitor   nonPollingMonitor1 {7}:  ON HOLD  --> TCM             (Sent)
Monitor   nonPollingMonitor1 {8}:  ON HOLD  --> TCM             (Sent)
Monitor   nonPollingMonitor2 {9}:  ON HOLD  --> TCM             (Sent)
  Success   run-auto,wait {1}:
Command   ACT-nonPollingMonitor1 {11}:        TCM {5} --> ON HOLD         (Inactive)
Command   ACT-nonPollingMonitor1 {12}:        TCM {3} --> ON HOLD         (Inactive)
Command   ACT-nonPollingMonitor2 {13}:        TCM {9} --> ON HOLD         (Inactive)
Command   ACT-nonPollingMonitor2 {14}:        TCM {9} --> ON HOLD         (Inactive)
Command   ACT-nonPollingMonitor1 {11}:  ON HOLD  --> TCM             (Sent)
 Complete nonPollingMonitor1 {5}:
Command   ACT-nonPollingMonitor1 {12}:  ON HOLD  --> TCM             (Sent)
 Complete nonPollingMonitor1 {3}:
Command   ACT-nonPollingMonitor2 {13}:  ON HOLD  --> TCM             (Sent)
 Complete nonPollingMonitor2 {9}:
Command   ACT-nonPollingMonitor1 {15}:        TCM {4} --> ON HOLD         (Inactive)
Terminated bar {2}
Command   ACT-nonPollingMonitor1 {15}:  ON HOLD  --> TCM             (Sent)
 Complete nonPollingMonitor1 {4}:
Terminated bar {10}
Test-NonPollingMonitor1:  [t=0, a=1]   ( 2 , "with activate at 0:0:01.0" )
  Success ACT-nonPollingMonitor1 {11}:
Test-NonPollingMonitor1:  [t=0, a=1]   ( 0 , "with activate" )
  Success ACT-nonPollingMonitor1 {12}:
Test-NonPollingMonitor2:  [t=0, a=2]   ( 5 , "without a with clause" )
  Success ACT-nonPollingMonitor2 {13}:
Command   ACT-nonPollingMonitor2 {14}:  ON HOLD  --> TCM             (Sent)
Test-NonPollingMonitor1:  [t=0, a=1]   ( 1 , "with activate at b1 handling completed" )
  Success ACT-nonPollingMonitor1 {15}:
Test-NonPollingMonitor2:  [t=1, a=2]   ( 5 , "without a with clause" )
  Success ACT-nonPollingMonitor2 {14}:
Command   ACT-nonPollingMonitor1 {16}:        TCM {7} --> ON HOLD         (Inactive)
Command   ACT-nonPollingMonitor1 {16}:  ON HOLD  --> TCM             (Sent)
 Complete nonPollingMonitor1 {7}:
Test-NonPollingMonitor1:  [t=0, a=1]   ( 3 , "with activate in 0:0:2.3" )
  Success ACT-nonPollingMonitor1 {16}:
Command   ACT-nonPollingMonitor1 {17}:        TCM {8} --> ON HOLD         (Inactive)
Command   ACT-nonPollingMonitor1 {17}:  ON HOLD  --> TCM             (Sent)
 Complete nonPollingMonitor1 {8}:
Test-NonPollingMonitor1:  [t=0, a=1]   ( 4 , "with activate in 0:0:3.1 after b1 handling completed" )
  Success ACT-nonPollingMonitor1 {17}:
