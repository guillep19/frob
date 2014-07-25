Task Control Management x.y.z (MON-DAY-YEAR)
Goal        foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {2}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-1 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {3}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-1"
_TDL_Serial ( Constrain=f-2 , Ref=f-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {4}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-2"
_TDL_Serial ( Constrain=f-5 , Ref=f-2 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {5}:        TCM {1} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-5"
_TDL_Serial ( Constrain=f-16 , Ref=f-5 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {6}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    f {2}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {1}:
test-f: START foo
  Success               f {2}:
Goal                    f {3}:  ON HOLD  --> TCM             (Sent)
test-f: start i=0
  Success               f {3}:
Goal                    f {4}:  ON HOLD  --> TCM             (Sent)
test-f: j=0
  Success               f {4}:
Goal                    f {5}:  ON HOLD  --> TCM             (Sent)
test-f: end i=0
  Success               f {5}:
Goal                    f {6}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {6}:
test-0-0
Goal        foo-auto,wait {7}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {7}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {8}:        TCM {7} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-1 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {9}:        TCM {7} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-1"
_TDL_Serial ( Constrain=f-3 , Ref=f-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {10}:        TCM {7} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-3"
_TDL_Serial ( Constrain=f-4 , Ref=f-3 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {11}:        TCM {7} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-4"
_TDL_Serial ( Constrain=f-5 , Ref=f-4 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {12}:        TCM {7} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-5"
_TDL_Serial ( Constrain=f-16 , Ref=f-5 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {13}:        TCM {7} --> ON HOLD         (Inactive)
Goal                    f {8}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {7}:
test-f: START foo
  Success               f {8}:
Goal                    f {9}:  ON HOLD  --> TCM             (Sent)
test-f: start i=0
  Success               f {9}:
Goal                    f {10}:  ON HOLD  --> TCM             (Sent)
test-f: j=1
  Success               f {10}:
Goal                    f {11}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {11}:
Goal                    f {12}:  ON HOLD  --> TCM             (Sent)
test-f: end i=0
  Success               f {12}:
Goal                    f {13}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {13}:
test-0-1
Goal        foo-auto,wait {14}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {14}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {15}:        TCM {14} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-1 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {16}:        TCM {14} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-1"
_TDL_Serial ( Constrain=f-4 , Ref=f-1 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {17}:        TCM {14} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-4"
_TDL_Serial ( Constrain=f-5 , Ref=f-4 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {18}:        TCM {14} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-5"
_TDL_Serial ( Constrain=f-16 , Ref=f-5 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {19}:        TCM {14} --> ON HOLD         (Inactive)
Goal                    f {15}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {14}:
test-f: START foo
  Success               f {15}:
Goal                    f {16}:  ON HOLD  --> TCM             (Sent)
test-f: start i=0
  Success               f {16}:
Goal                    f {17}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {17}:
Goal                    f {18}:  ON HOLD  --> TCM             (Sent)
test-f: end i=0
  Success               f {18}:
Goal                    f {19}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {19}:
test-0-2
Goal        foo-auto,wait {20}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {20}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {21}:        TCM {20} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-6 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {22}:        TCM {20} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-6"
_TDL_Serial ( Constrain=f-7 , Ref=f-6 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {23}:        TCM {20} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-7"
_TDL_Serial ( Constrain=f-10 , Ref=f-7 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {24}:        TCM {20} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-10"
_TDL_Serial ( Constrain=f-11 , Ref=f-10 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {25}:        TCM {20} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-11"
_TDL_Serial ( Constrain=f-12 , Ref=f-11 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {26}:        TCM {20} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-12"
_TDL_Serial ( Constrain=f-15 , Ref=f-12 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {27}:        TCM {20} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-15"
_TDL_Serial ( Constrain=f-16 , Ref=f-15 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {28}:        TCM {20} --> ON HOLD         (Inactive)
Goal                    f {21}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {20}:
test-f: START foo
  Success               f {21}:
Goal                    f {22}:  ON HOLD  --> TCM             (Sent)
test-f: start i=1
  Success               f {22}:
Goal                    f {23}:  ON HOLD  --> TCM             (Sent)
test-f: j=0
  Success               f {23}:
Goal                    f {24}:  ON HOLD  --> TCM             (Sent)
test-f: end i=1
  Success               f {24}:
Goal                    f {25}:  ON HOLD  --> TCM             (Sent)
test-f: start i=default
  Success               f {25}:
Goal                    f {26}:  ON HOLD  --> TCM             (Sent)
test-f: j=0
  Success               f {26}:
Goal                    f {27}:  ON HOLD  --> TCM             (Sent)
test-f: end i=default
  Success               f {27}:
Goal                    f {28}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {28}:
test-1-0
Goal        foo-auto,wait {29}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {29}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {30}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-6 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {31}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-6"
_TDL_Serial ( Constrain=f-8 , Ref=f-6 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {32}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-8"
_TDL_Serial ( Constrain=f-9 , Ref=f-8 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {33}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-9"
_TDL_Serial ( Constrain=f-10 , Ref=f-9 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {34}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-10"
_TDL_Serial ( Constrain=f-11 , Ref=f-10 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {35}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-11"
_TDL_Serial ( Constrain=f-13 , Ref=f-11 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {36}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-13"
_TDL_Serial ( Constrain=f-14 , Ref=f-13 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {37}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-14"
_TDL_Serial ( Constrain=f-15 , Ref=f-14 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {38}:        TCM {29} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-15"
_TDL_Serial ( Constrain=f-16 , Ref=f-15 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {39}:        TCM {29} --> ON HOLD         (Inactive)
Goal                    f {30}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {29}:
test-f: START foo
  Success               f {30}:
Goal                    f {31}:  ON HOLD  --> TCM             (Sent)
test-f: start i=1
  Success               f {31}:
Goal                    f {32}:  ON HOLD  --> TCM             (Sent)
test-f: j=1
  Success               f {32}:
Goal                    f {33}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {33}:
Goal                    f {34}:  ON HOLD  --> TCM             (Sent)
test-f: end i=1
  Success               f {34}:
Goal                    f {35}:  ON HOLD  --> TCM             (Sent)
test-f: start i=default
  Success               f {35}:
Goal                    f {36}:  ON HOLD  --> TCM             (Sent)
test-f: j=1
  Success               f {36}:
Goal                    f {37}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {37}:
Goal                    f {38}:  ON HOLD  --> TCM             (Sent)
test-f: end i=default
  Success               f {38}:
Goal                    f {39}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {39}:
test-1-1
Goal        foo-auto,wait {40}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {40}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {41}:        TCM {40} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-6 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {42}:        TCM {40} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-6"
_TDL_Serial ( Constrain=f-9 , Ref=f-6 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {43}:        TCM {40} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-9"
_TDL_Serial ( Constrain=f-10 , Ref=f-9 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {44}:        TCM {40} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-10"
_TDL_Serial ( Constrain=f-11 , Ref=f-10 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {45}:        TCM {40} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-11"
_TDL_Serial ( Constrain=f-14 , Ref=f-11 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {46}:        TCM {40} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-14"
_TDL_Serial ( Constrain=f-15 , Ref=f-14 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {47}:        TCM {40} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-15"
_TDL_Serial ( Constrain=f-16 , Ref=f-15 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {48}:        TCM {40} --> ON HOLD         (Inactive)
Goal                    f {41}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {40}:
test-f: START foo
  Success               f {41}:
Goal                    f {42}:  ON HOLD  --> TCM             (Sent)
test-f: start i=1
  Success               f {42}:
Goal                    f {43}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {43}:
Goal                    f {44}:  ON HOLD  --> TCM             (Sent)
test-f: end i=1
  Success               f {44}:
Goal                    f {45}:  ON HOLD  --> TCM             (Sent)
test-f: start i=default
  Success               f {45}:
Goal                    f {46}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {46}:
Goal                    f {47}:  ON HOLD  --> TCM             (Sent)
test-f: end i=default
  Success               f {47}:
Goal                    f {48}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {48}:
test-1-2
Goal        foo-auto,wait {49}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {49}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {50}:        TCM {49} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-11 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {51}:        TCM {49} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-11"
_TDL_Serial ( Constrain=f-12 , Ref=f-11 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {52}:        TCM {49} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-12"
_TDL_Serial ( Constrain=f-15 , Ref=f-12 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {53}:        TCM {49} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-15"
_TDL_Serial ( Constrain=f-16 , Ref=f-15 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {54}:        TCM {49} --> ON HOLD         (Inactive)
Goal                    f {50}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {49}:
test-f: START foo
  Success               f {50}:
Goal                    f {51}:  ON HOLD  --> TCM             (Sent)
test-f: start i=default
  Success               f {51}:
Goal                    f {52}:  ON HOLD  --> TCM             (Sent)
test-f: j=0
  Success               f {52}:
Goal                    f {53}:  ON HOLD  --> TCM             (Sent)
test-f: end i=default
  Success               f {53}:
Goal                    f {54}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {54}:
test-2-0
Goal        foo-auto,wait {55}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {55}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {56}:        TCM {55} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-11 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {57}:        TCM {55} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-11"
_TDL_Serial ( Constrain=f-13 , Ref=f-11 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {58}:        TCM {55} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-13"
_TDL_Serial ( Constrain=f-14 , Ref=f-13 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {59}:        TCM {55} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-14"
_TDL_Serial ( Constrain=f-15 , Ref=f-14 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {60}:        TCM {55} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-15"
_TDL_Serial ( Constrain=f-16 , Ref=f-15 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {61}:        TCM {55} --> ON HOLD         (Inactive)
Goal                    f {56}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {55}:
test-f: START foo
  Success               f {56}:
Goal                    f {57}:  ON HOLD  --> TCM             (Sent)
test-f: start i=default
  Success               f {57}:
Goal                    f {58}:  ON HOLD  --> TCM             (Sent)
test-f: j=1
  Success               f {58}:
Goal                    f {59}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {59}:
Goal                    f {60}:  ON HOLD  --> TCM             (Sent)
test-f: end i=default
  Success               f {60}:
Goal                    f {61}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {61}:
test-2-1
Goal        foo-auto,wait {62}:        TCM {0} --> ON HOLD         (Inactive)
Goal        foo-auto,wait {62}:  ON HOLD  --> TCM             (Sent)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "NULL"
[_TDL_Constraint::findPrevious]  TDL_DEBUG:  [With-Do]   No previous nodes for available for "f-0" Constraint:
   Constraint:  _TDL_Serial  (0x........)
    ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {63}:        TCM {62} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-0"
_TDL_Serial ( Constrain=f-11 , Ref=f-0 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {64}:        TCM {62} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-11"
_TDL_Serial ( Constrain=f-14 , Ref=f-11 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {65}:        TCM {62} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-14"
_TDL_Serial ( Constrain=f-15 , Ref=f-14 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {66}:        TCM {62} --> ON HOLD         (Inactive)
[_TDL_TreeNodeBranch::getPreviousChild]  TDL_DEBUG:  Returning Previous of:  "f-15"
_TDL_Serial ( Constrain=f-16 , Ref=f-15 [Ref_flags: RUNNING] )
Constraint:  _TDL_Serial  (0x........)
 ActualReferenceNode = 0x........   ("PREVIOUS")

Goal                    f {67}:        TCM {62} --> ON HOLD         (Inactive)
Goal                    f {63}:  ON HOLD  --> TCM             (Sent)
  Success   foo-auto,wait {62}:
test-f: START foo
  Success               f {63}:
Goal                    f {64}:  ON HOLD  --> TCM             (Sent)
test-f: start i=default
  Success               f {64}:
Goal                    f {65}:  ON HOLD  --> TCM             (Sent)
test-f: j=default
  Success               f {65}:
Goal                    f {66}:  ON HOLD  --> TCM             (Sent)
test-f: end i=default
  Success               f {66}:
Goal                    f {67}:  ON HOLD  --> TCM             (Sent)
test-f: END foo
  Success               f {67}:
test-2-2
