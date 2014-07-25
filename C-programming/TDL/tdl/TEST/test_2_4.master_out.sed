Task Control Management x.y.z (MON-DAY-YEAR)
test1:  
0x........

Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
test-foo-pre-f1
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f1-0" Constraint:
   Constraint:  _TDL_SequentialExpansion  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f1-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f1-0" Constraint:
   Constraint:  _TDL_SequentialHandling  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                   f1 {2}:        TCM {1} --> ON HOLD         (Inactive)
test-foo-post-f1-pre-f2
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f1-0"
_TDL_SequentialExpansion ( Constrain=f2-0 , Ref=f1-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExpansion  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f1-0"
_TDL_Serial ( Constrain=f2-0 , Ref=f1-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f2-0" Constraint:
   Constraint:  _TDL_SequentialHandling  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                   f2 {3}:        TCM {1} --> ON HOLD         (Inactive)
test-foo-post-f2
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "WithDo-1"
_TDL_SequentialExecution ( Constrain=f3-0 , Ref=f1-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SequentialExecution ( Constrain=f3-0 , Ref=f2-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialExecution  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "WithDo-1"
_TDL_SequentialHandling ( Constrain=f3-0 , Ref=f1-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

_TDL_SequentialHandling ( Constrain=f3-0 , Ref=f2-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_SequentialHandling  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                   f3 {4}:        TCM {1} --> ON HOLD         (Inactive)
test-foo-post-f3
Goal                   f1 {2}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
test-f1
  Success              f1 {2}:
Goal                   f2 {3}:  ON HOLD  --> TCM             (Sent)
test-f2
  Success              f2 {3}:
Goal                   f3 {4}:  ON HOLD  --> TCM             (Sent)
test-f3
  Success              f3 {4}:
