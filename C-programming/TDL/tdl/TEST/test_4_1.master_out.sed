Task Control Management x.y.z (MON-DAY-YEAR)
Goal         f1-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal         f1-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
test-f1 ( 1 )
Goal                   f2 {2}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f2-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f2 {2}:  ON HOLD  --> TCM             (Sent)
test-f2 ( 2 )
Goal                   f3 {3}:        TCM {2} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f3-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f3 {3}:  ON HOLD  --> TCM             (Sent)
test-f3 ( 3 )
Goal         engineOnFire {4}:        TCM {3} --> ON HOLD         (Inactive)
Goal         engineOnFire {4}:  ON HOLD  --> TCM             (Sent)
  Failure              f3 {3}:
test-engineOnFireHandler ( f3 ) ==> f3-engineOnFire - 3
Goal         engineOnFire {5}:        TCM {4} --> ON HOLD         (Inactive)
Goal         engineOnFire {5}:  ON HOLD  --> TCM             (Sent)
  Success    engineOnFire {4}:
test-engineFailureHandler ( f2 ) ==> f3-engineOnFire - 3
Goal         engineOnFire {6}:        TCM {5} --> ON HOLD         (Inactive)
Goal         engineOnFire {6}:  ON HOLD  --> TCM             (Sent)
  Success    engineOnFire {5}:
test-hardwareFailureHandler ( f1 ) ==> f3-engineOnFire - f3-engineOnFire
  Success    engineOnFire {6}:
Goal                   f4 {7}:        TCM {2} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f4-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f4 {7}:  ON HOLD  --> TCM             (Sent)
test-f4 ( 4 )
Goal        engineFailure {8}:        TCM {7} --> ON HOLD         (Inactive)
Goal        engineFailure {8}:  ON HOLD  --> TCM             (Sent)
  Failure              f4 {7}:
test-engineFailureHandler ( f2 ) ==> f4-engineFailure - 4
Goal        engineFailure {9}:        TCM {8} --> ON HOLD         (Inactive)
Goal        engineFailure {9}:  ON HOLD  --> TCM             (Sent)
  Success   engineFailure {8}:
test-hardwareFailureHandler ( f1 ) ==> f4-engineFailure - f4-engineFailure
  Success   engineFailure {9}:
  Success              f2 {2}:
Goal                   f5 {10}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f5-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f5 {10}:  ON HOLD  --> TCM             (Sent)
test-f5 ( 13 )
Goal                   f6 {11}:        TCM {10} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f6-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f6 {11}:  ON HOLD  --> TCM             (Sent)
test-f6 ( 14 )
Goal      hardwareFailure {12}:        TCM {11} --> ON HOLD         (Inactive)
Goal      hardwareFailure {12}:  ON HOLD  --> TCM             (Sent)
  Failure              f6 {11}:
test-hardwareFailureHandler ( f1 ) ==> f6-hardwareFailure - f6-hardwareFailure
  Success hardwareFailure {12}:
  Success              f5 {10}:
Goal                   f7 {13}:        TCM {1} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f7-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f7 {13}:  ON HOLD  --> TCM             (Sent)
test-f7 ( 24 )
Goal                   f8 {14}:        TCM {13} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=f8-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                   f8 {14}:  ON HOLD  --> TCM             (Sent)
test-f8 ( 25 )
Goal         engineOnFire {15}:        TCM {14} --> ON HOLD         (Inactive)
Goal         engineOnFire {15}:  ON HOLD  --> TCM             (Sent)
  Failure              f8 {14}:
test-hardwareFailureHandler ( f1 ) ==> f8-engineOnFire - f8-engineOnFire
  Success    engineOnFire {15}:
  Success              f7 {13}:
  Success    f1-auto,wait {1}:
