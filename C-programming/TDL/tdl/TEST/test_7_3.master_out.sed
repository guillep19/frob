Task Control Management x.y.z (MON-DAY-YEAR)
Goal       test-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 0 0] ,  Ref=aMonitor-0[. 0 0 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [Spawn]   No previous nodes for available for "aMonitor-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 0], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 0], maximum_activates = 4 )
Monitor          aMonitor {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 0 1] ,  Ref=aMonitor-0[. 0 0 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 0 1] , Ref=aMonitor-0[. 0 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 1], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 1], maximum_activates = 4 )
Monitor          aMonitor {4}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 0 2] ,  Ref=aMonitor-0[. 0 0 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 0 2] , Ref=aMonitor-0[. 0 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 2], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 0 2], maximum_activates = 4 )
Monitor          aMonitor {5}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 1 0] ,  Ref=aMonitor-0[. 0 1 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 1 0] , Ref=aMonitor-0[. 0 0 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 0], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 0], maximum_activates = 4 )
Monitor          aMonitor {6}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 1 1] ,  Ref=aMonitor-0[. 0 1 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 1 1] , Ref=aMonitor-0[. 0 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 1], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 1], maximum_activates = 4 )
Monitor          aMonitor {7}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 1 2] ,  Ref=aMonitor-0[. 0 1 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 1 2] , Ref=aMonitor-0[. 0 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 2], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 1 2], maximum_activates = 4 )
Monitor          aMonitor {8}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 2 0] ,  Ref=aMonitor-0[. 0 2 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 2 0] , Ref=aMonitor-0[. 0 1 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 0], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 0], maximum_activates = 4 )
Monitor          aMonitor {9}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 2 1] ,  Ref=aMonitor-0[. 0 2 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 2 1] , Ref=aMonitor-0[. 0 2 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 1], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 1], maximum_activates = 4 )
Monitor          aMonitor {10}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 0 2 2] ,  Ref=aMonitor-0[. 0 2 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 0 2 2] , Ref=aMonitor-0[. 0 2 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 2], maximum_activates = 6 )
Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 0 2 2], maximum_activates = 4 )
Monitor          aMonitor {11}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 0 0] ,  Ref=aMonitor-0[. 1 0 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 0 0] , Ref=aMonitor-0[. 0 2 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 0 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 0 0], maximum_activates = 4 )
Monitor          aMonitor {12}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 0 1] ,  Ref=aMonitor-0[. 1 0 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 0 1] , Ref=aMonitor-0[. 1 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 0 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 0 1], maximum_activates = 4 )
Monitor          aMonitor {13}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 0 2] ,  Ref=aMonitor-0[. 1 0 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 0 2] , Ref=aMonitor-0[. 1 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 0 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 0 2], maximum_activates = 4 )
Monitor          aMonitor {14}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 1 0] ,  Ref=aMonitor-0[. 1 1 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 1 0] , Ref=aMonitor-0[. 1 0 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 1 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 1 0], maximum_activates = 4 )
Monitor          aMonitor {15}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 1 1] ,  Ref=aMonitor-0[. 1 1 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 1 1] , Ref=aMonitor-0[. 1 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 1 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 1 1], maximum_activates = 4 )
Monitor          aMonitor {16}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 1 2] ,  Ref=aMonitor-0[. 1 1 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 1 2] , Ref=aMonitor-0[. 1 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 1 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 1 2], maximum_activates = 4 )
Monitor          aMonitor {17}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 2 0] ,  Ref=aMonitor-0[. 1 2 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 2 0] , Ref=aMonitor-0[. 1 1 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 2 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 2 0], maximum_activates = 4 )
Monitor          aMonitor {18}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 2 1] ,  Ref=aMonitor-0[. 1 2 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 2 1] , Ref=aMonitor-0[. 1 2 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 2 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 2 1], maximum_activates = 4 )
Monitor          aMonitor {19}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 1 2 2] ,  Ref=aMonitor-0[. 1 2 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 1 2 2] , Ref=aMonitor-0[. 1 2 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 2 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 1 2 2], maximum_activates = 4 )
Monitor          aMonitor {20}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 0 0] ,  Ref=aMonitor-0[. 2 0 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 0 0] , Ref=aMonitor-0[. 1 2 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 0 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 0 0], maximum_activates = 4 )
Monitor          aMonitor {21}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 0 1] ,  Ref=aMonitor-0[. 2 0 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 0 1] , Ref=aMonitor-0[. 2 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 0 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 0 1], maximum_activates = 4 )
Monitor          aMonitor {22}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 0 2] ,  Ref=aMonitor-0[. 2 0 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 0 2] , Ref=aMonitor-0[. 2 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 0 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 0 2], maximum_activates = 4 )
Monitor          aMonitor {23}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 1 0] ,  Ref=aMonitor-0[. 2 1 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 1 0] , Ref=aMonitor-0[. 2 0 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 1 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 1 0], maximum_activates = 4 )
Monitor          aMonitor {24}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 1 1] ,  Ref=aMonitor-0[. 2 1 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 1 1] , Ref=aMonitor-0[. 2 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 1 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 1 1], maximum_activates = 4 )
Monitor          aMonitor {25}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 1 2] ,  Ref=aMonitor-0[. 2 1 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 1 2] , Ref=aMonitor-0[. 2 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 1 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 1 2], maximum_activates = 4 )
Monitor          aMonitor {26}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 2 0] ,  Ref=aMonitor-0[. 2 2 0] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 2 0] , Ref=aMonitor-0[. 2 1 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 2 0], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 2 0], maximum_activates = 4 )
Monitor          aMonitor {27}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 2 1] ,  Ref=aMonitor-0[. 2 2 1] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 2 1] , Ref=aMonitor-0[. 2 2 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 2 1], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 2 1], maximum_activates = 4 )
Monitor          aMonitor {28}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_TerminateInTime ( Constrain=aMonitor-0[. 2 2 2] ,  Ref=aMonitor-0[. 2 2 2] [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 500  (MSecs) )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 500  (MSecs)
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "aMonitor-0"
_TDL_Serial ( Constrain=aMonitor-0[. 2 2 2] , Ref=aMonitor-0[. 2 2 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 2 2], maximum_activates = 1 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 1

Constraint overriden (and canceled):    _TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 2 2], maximum_activates = 4 )
Monitor          aMonitor {29}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_SetMonitorMaximumActivations ( Constrain=aMonitor-0[. 2 1 1], maximum_activates = 2 )
Constraint:  _TDL_SetMonitorMaximumActivations  (0x........)
 Maximum Activates = 2

_TDL_TerminateInTime ( Constrain=aMonitor-1 ,  Ref=aMonitor-1 [Ref_flags: ALLOCATED]  , RefInterval=**UNKNOWN_INTERVAL** , RefState=ENABLED_STATE ,  Time= 0 : 0 : 3 . 0 )
Constraint:  _TDL_TerminateInTime  (0x........)
 Time= 0 : 0 : 3 . 0
 referenceInterval = Unknown Interval
 referenceState = Enabled State
 ActualReferenceNode = 0x........   ("CHILD (SELF)")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "WithDo-0"
_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 0 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 1 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 2 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 2 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 0 2 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 0 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 1 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 2 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 2 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 1 2 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 0 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 0 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 0 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 1 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 1 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 1 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 2 0] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 2 1] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_Serial ( Constrain=aMonitor-1 , Ref=aMonitor-0[. 2 2 2] [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Monitor          aMonitor {30}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_ActivateImmediate ( Constrain=task1 )
Constraint:  _TDL_ActivateImmediate  (0x........)

_TDL_ActivateImmediate ( Constrain=task1 )
Constraint:  _TDL_ActivateImmediate  (0x........)

_TDL_ActivateImmediate ( Constrain=task1 )
Constraint:  _TDL_ActivateImmediate  (0x........)

Monitor          aMonitor {2}:  ON HOLD  --> TCM             (Sent)
  Success  test-auto,wait {1}:
Will Terminate aMonitor {2} when all references to it are released
Monitor          aMonitor {4}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {2}
Will Terminate aMonitor {4} when all references to it are released
Monitor          aMonitor {5}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {4}
Will Terminate aMonitor {5} when all references to it are released
Monitor          aMonitor {6}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {5}
Will Terminate aMonitor {6} when all references to it are released
Monitor          aMonitor {7}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {6}
Will Terminate aMonitor {7} when all references to it are released
Monitor          aMonitor {8}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {7}
Will Terminate aMonitor {8} when all references to it are released
Monitor          aMonitor {9}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {8}
Will Terminate aMonitor {9} when all references to it are released
Monitor          aMonitor {10}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {9}
Will Terminate aMonitor {10} when all references to it are released
Monitor          aMonitor {11}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {10}
Will Terminate aMonitor {11} when all references to it are released
Monitor          aMonitor {12}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {11}
Will Terminate aMonitor {12} when all references to it are released
Monitor          aMonitor {13}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {12}
Will Terminate aMonitor {13} when all references to it are released
Monitor          aMonitor {14}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {13}
Will Terminate aMonitor {14} when all references to it are released
Monitor          aMonitor {15}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {14}
Will Terminate aMonitor {15} when all references to it are released
Monitor          aMonitor {16}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {15}
Will Terminate aMonitor {16} when all references to it are released
Monitor          aMonitor {17}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {16}
Will Terminate aMonitor {17} when all references to it are released
Monitor          aMonitor {18}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {17}
Command      ACT-aMonitor {31}:        TCM {18} --> ON HOLD         (Inactive)
Command      ACT-aMonitor {31}:  ON HOLD  --> TCM             (Sent)
 Complete        aMonitor {18}:
Test(1,2,0):  aMonitor {1, 4294967295, 0, INFINITE_TIME, 0)
  Success    ACT-aMonitor {31}:
Monitor          aMonitor {19}:  ON HOLD  --> TCM             (Sent)
Will Terminate aMonitor {19} when all references to it are released
Monitor          aMonitor {20}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {19}
Will Terminate aMonitor {20} when all references to it are released
Monitor          aMonitor {21}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {20}
Will Terminate aMonitor {21} when all references to it are released
Monitor          aMonitor {22}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {21}
Will Terminate aMonitor {22} when all references to it are released
Monitor          aMonitor {23}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {22}
Will Terminate aMonitor {23} when all references to it are released
Monitor          aMonitor {24}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {23}
Will Terminate aMonitor {24} when all references to it are released
Monitor          aMonitor {25}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {24}
Command      ACT-aMonitor {32}:        TCM {25} --> ON HOLD         (Inactive)
Command      ACT-aMonitor {32}:  ON HOLD  --> TCM             (Sent)
Test(2,1,1):  aMonitor {2, 4294967295, 0, INFINITE_TIME, 0)
  Success    ACT-aMonitor {32}:
Will Terminate aMonitor {25} when all references to it are released
Monitor          aMonitor {26}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {25}
Will Terminate aMonitor {26} when all references to it are released
Monitor          aMonitor {27}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {26}
Will Terminate aMonitor {27} when all references to it are released
Monitor          aMonitor {28}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {27}
Will Terminate aMonitor {28} when all references to it are released
Monitor          aMonitor {29}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {28}
Will Terminate aMonitor {29} when all references to it are released
Monitor          aMonitor {30}:  ON HOLD  --> TCM             (Sent)
Terminated aMonitor {29}
Command      ACT-aMonitor {33}:        TCM {30} --> ON HOLD         (Inactive)
Command      ACT-aMonitor {33}:  ON HOLD  --> TCM             (Sent)
Test(-1,-1,-1):  aMonitor {4294967295, 4294967295, 0, INFINITE_TIME, 0)
  Success    ACT-aMonitor {33}:
Will Terminate aMonitor {30} when all references to it are released
Terminated aMonitor {30}
