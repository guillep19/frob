Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_SequentialHandling ( Constrain=f-1 , Ref=f-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_SequentialHandling:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-1" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExpansion ( Constrain=f-2 , Ref=f-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_SequentialExpansion:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-1"
_TDL_Serial ( Constrain=f-2 , Ref=f-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {3}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SequentialExecution ( Constrain=f-3 , Ref=f-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_SequentialExecution:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-2"
_TDL_Serial ( Constrain=f-3 , Ref=f-2 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Serial ( Constrain=f-4 , Ref=f-0 [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-3"
_TDL_Serial ( Constrain=f-4 , Ref=f-3 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {5}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_DisableUntilEvent ( Constrain=f-5 , ConstrainInterval=**UNKNOWN_INTERVAL** , Ref=f-0 [Ref_flags: DESTROYED] , RefInterval=HANDLING_INTERVAL , RefState=COMPLETED_STATE )
Constraint:  _TDL_DisableUntilEvent  (0x........)
 NodeToConstrainInterval = Unknown Interval
 referenceInterval = Handling Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_DisableUntilEvent:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-4"
_TDL_Serial ( Constrain=f-5 , Ref=f-4 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {6}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_DisableForTime ( Constrain=f-6 , ConstrainInterval=**UNKNOWN_INTERVAL** ,  [****REF DESTROYED*****]  Ref=f-0 [Ref_flags: DESTROYED]  , RefInterval=EXPANSION_INTERVAL , RefState=ENABLED_STATE ,  Time= 0 : 0 : 5 . 0.2 )
Constraint:  _TDL_DisableForTime  (0x........)
 NodeToConstrainInterval = Unknown Interval
 Time= 0 : 0 : 5 . 0.2
 referenceInterval = Expansion Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_DisableForTime:performConstraint]  Warning:  "AFTER <event>"'s reference-node is destroyed.  AFTER clause ignored.
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-5"
_TDL_Serial ( Constrain=f-6 , Ref=f-5 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {7}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateAtEvent ( Constrain=f-7 , Ref=f-0 [Ref_flags: DESTROYED] , RefInterval=EXECUTION_INTERVAL , RefState=COMPLETED_STATE )
Constraint:  _TDL_TerminateAtEvent  (0x........)
 referenceInterval = Execution Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_TerminateAtEvent:performConstraint]  Error:  The Reference Task is "DESTROYED".  Most likely, Task "f-0" exists inside a conditional statement (IF or SWITCH) whose predicate evaluated to FALSE.  This TerminateAtEvent constraint will therefore terminate the Current Task ("f-7")  IN ZERO TIME.
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-6"
_TDL_Serial ( Constrain=f-7 , Ref=f-6 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {9}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=f-8 ,  [****REF DESTROYED*****]  Ref=f-0 [Ref_flags: DESTROYED]  , RefInterval=EXECUTION_INTERVAL , RefState=ENABLED_STATE ,  Time= 0 : 0 : 15 . 0.2 )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 0 : 0 : 15 . 0.2
 referenceInterval = Execution Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("f-0")

[_TDL_TerminateInTime:performConstraint]  Warning:  "AFTER <event>"'s reference-node is destroyed.  AFTER clause ignored.
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-7"
_TDL_Serial ( Constrain=f-8 , Ref=f-7 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {10}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_SequentialHandling ( Constrain=f-9 , Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_SequentialHandling:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-8"
_TDL_Serial ( Constrain=f-9 , Ref=f-8 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {11}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_SequentialExpansion ( Constrain=f-10 , Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_SequentialExpansion:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-9"
_TDL_Serial ( Constrain=f-10 , Ref=f-9 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {12}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_SequentialExecution ( Constrain=f-11 , Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_SequentialExecution:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-10"
_TDL_Serial ( Constrain=f-11 , Ref=f-10 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {13}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_Serial ( Constrain=f-12 , Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_Serial:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-11"
_TDL_Serial ( Constrain=f-12 , Ref=f-11 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {14}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_DisableUntilEvent ( Constrain=f-13 , ConstrainInterval=**UNKNOWN_INTERVAL** , Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED] , RefInterval=HANDLING_INTERVAL , RefState=COMPLETED_STATE )
Constraint:  _TDL_DisableUntilEvent  (0x........)
 NodeToConstrainInterval = Unknown Interval
 referenceInterval = Handling Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_DisableUntilEvent:performConstraint]  Warning:  theReferenceTreeNode is destroyed.  Skipping Constraint...
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-12"
_TDL_Serial ( Constrain=f-13 , Ref=f-12 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {15}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_DisableForTime ( Constrain=f-14 , ConstrainInterval=**UNKNOWN_INTERVAL** ,  [****REF DESTROYED*****]  Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED]  , RefInterval=EXPANSION_INTERVAL , RefState=ENABLED_STATE ,  Time= 0 : 0 : 5 . 0.2 )
Constraint:  _TDL_DisableForTime  (0x........)
 NodeToConstrainInterval = Unknown Interval
 Time= 0 : 0 : 5 . 0.2
 referenceInterval = Expansion Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_DisableForTime:performConstraint]  Warning:  "AFTER <event>"'s reference-node is destroyed.  AFTER clause ignored.
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-13"
_TDL_Serial ( Constrain=f-14 , Ref=f-13 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {16}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_TerminateAtEvent ( Constrain=f-15 , Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED] , RefInterval=EXECUTION_INTERVAL , RefState=COMPLETED_STATE )
Constraint:  _TDL_TerminateAtEvent  (0x........)
 referenceInterval = Execution Interval
 referenceState = Completed State
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_TerminateAtEvent:performConstraint]  Error:  The Reference Task is "DESTROYED".  Most likely, Task "_TDL_Constraint::PREVIOUS" exists inside a conditional statement (IF or SWITCH) whose predicate evaluated to FALSE.  This TerminateAtEvent constraint will therefore terminate the Current Task ("f-15")  IN ZERO TIME.
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-14"
_TDL_Serial ( Constrain=f-15 , Ref=f-14 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {17}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_ConstraintWithReference::recursivelyPerformConstraints]  Warning:  EMPTY REFERENCE contains no _TDL_SpawnStatementTreeNode objects.
_TDL_TerminateInTime ( Constrain=f-16 ,  [****REF DESTROYED*****]  Ref=_TDL_Constraint::PREVIOUS [Ref_flags: DESTROYED]  , RefInterval=EXECUTION_INTERVAL , RefState=ENABLED_STATE ,  Time= 0 : 0 : 15 . 0.2 )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 0 : 0 : 15 . 0.2
 referenceInterval = Execution Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("WithDo-1")

[_TDL_TerminateInTime:performConstraint]  Warning:  "AFTER <event>"'s reference-node is destroyed.  AFTER clause ignored.
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-15"
_TDL_Serial ( Constrain=f-16 , Ref=f-15 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {18}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    f {2}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
Will Terminate f {9} when all references to it are released
Will Terminate f {17} when all references to it are released
test-f: 0 - SEQUENTIAL HANDLING on if 0 spawn
  Success               f {2}:
Goal                    f {3}:  ON HOLD  --> TCM             (Sent)
test-f: 1 - SEQUENTIAL EXPANSION on if 0 spawn
  Success               f {3}:
Goal                    f {4}:  ON HOLD  --> TCM             (Sent)
test-f: 2 - SEQUENTIAL EXECUTION on if 0 spawn
  Success               f {4}:
Goal                    f {5}:  ON HOLD  --> TCM             (Sent)
test-f: 3 - SERIAL on if 0 spawn
  Success               f {5}:
Goal                    f {6}:  ON HOLD  --> TCM             (Sent)
test-f: 4 - DISABLE UNTIL (if 0 spawn) HANDLING COMPLETED
  Success               f {6}:
Goal                    f {7}:  ON HOLD  --> TCM             (Sent)
test-f: 5 - DISABLE FOR 0:0:5.2 AFTER (if 0 spawn) EXPANSION ENABLED
  Success               f {7}:
Goal                    f {10}:  ON HOLD  --> TCM             (Sent)
Terminated f {9}
test-f: 7 - TERMINATE IN 0:0:15.2 AFTER (if 0 spawn) EXECUTION ACTIVE
  Success               f {10}:
Goal                    f {11}:  ON HOLD  --> TCM             (Sent)
test-f: 8 - SEQUENTIAL HANDLING on if 0 with
  Success               f {11}:
Goal                    f {12}:  ON HOLD  --> TCM             (Sent)
test-f: 9 - SEQUENTIAL EXPANSION on if 0 with
  Success               f {12}:
Goal                    f {13}:  ON HOLD  --> TCM             (Sent)
test-f: 10 - SEQUENTIAL EXECUTION on if 0 with
  Success               f {13}:
Goal                    f {14}:  ON HOLD  --> TCM             (Sent)
test-f: 11 - SERIAL on if 0 with
  Success               f {14}:
Goal                    f {15}:  ON HOLD  --> TCM             (Sent)
test-f: 12 - DISABLE UNTIL (if 0 with) HANDLING COMPLETED
  Success               f {15}:
Goal                    f {16}:  ON HOLD  --> TCM             (Sent)
test-f: 13 - DISABLE FOR 0:0:5.2 AFTER (if 0 with) EXPANSION ENABLED
  Success               f {16}:
Goal                    f {18}:  ON HOLD  --> TCM             (Sent)
Terminated f {17}
test-f: 15 - TERMINATE IN 0:0:15.2 AFTER (if 0 with) EXECUTION ACTIVE
  Success               f {18}:
