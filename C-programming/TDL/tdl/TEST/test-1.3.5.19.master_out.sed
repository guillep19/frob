Task Control Management x.y.z (MON-DAY-YEAR)
Goal        run-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        run-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "foo-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                  foo {2}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "foo-0"
_TDL_Serial ( Constrain=foo-1 , Ref=foo-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal              foo-int {3}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "foo-1"
_TDL_Serial ( Constrain=foo-2 , Ref=foo-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal           foo-double {4}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "foo-2"
_TDL_Serial ( Constrain=bar-0 , Ref=foo-2 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Command               bar {5}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "bar-0"
_TDL_Serial ( Constrain=bar-1 , Ref=bar-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Command           bar-int {6}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "bar-1"
_TDL_Serial ( Constrain=bar-2 , Ref=bar-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Command        bar-double {7}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "bar-2"
_TDL_Serial ( Constrain=charlie-0 , Ref=bar-2 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Monitor           charlie {8}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "charlie-0"
_TDL_Serial ( Constrain=charlie-1 , Ref=charlie-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Monitor       charlie-int {9}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "charlie-1"
_TDL_Serial ( Constrain=charlie-2 , Ref=charlie-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Monitor    charlie-double {10}:        TCM {1} --> ON HOLD         (Inactive)
Goal                  foo {2}:  ON HOLD  --> TCM             (Sent)
  Success   run-auto,wait {1}:
foo()
  Success             foo {2}:
Goal              foo-int {3}:  ON HOLD  --> TCM             (Sent)
foo(int)
  Success         foo-int {3}:
Goal           foo-double {4}:  ON HOLD  --> TCM             (Sent)
foo(double)
  Success      foo-double {4}:
Command               bar {5}:  ON HOLD  --> TCM             (Sent)
bar()
  Success             bar {5}:
Command           bar-int {6}:  ON HOLD  --> TCM             (Sent)
bar(int)
  Success         bar-int {6}:
Command        bar-double {7}:  ON HOLD  --> TCM             (Sent)
bar(double)
  Success      bar-double {7}:
Monitor           charlie {8}:  ON HOLD  --> TCM             (Sent)
Command       ACT-charlie {11}:        TCM {8} --> ON HOLD         (Inactive)
Command       ACT-charlie {11}:  ON HOLD  --> TCM             (Sent)
 Complete         charlie {8}:
charlie()
  Success     ACT-charlie {11}:
Monitor       charlie-int {9}:  ON HOLD  --> TCM             (Sent)
Command   ACT-charlie-int {12}:        TCM {9} --> ON HOLD         (Inactive)
Command   ACT-charlie-int {12}:  ON HOLD  --> TCM             (Sent)
 Complete     charlie-int {9}:
charlie(int)
  Success ACT-charlie-int {12}:
Monitor    charlie-double {10}:  ON HOLD  --> TCM             (Sent)
Command   ACT-charlie-double {13}:        TCM {10} --> ON HOLD         (Inactive)
Command   ACT-charlie-double {13}:  ON HOLD  --> TCM             (Sent)
 Complete  charlie-double {10}:
charlie(double)
  Success ACT-charlie-double {13}:
