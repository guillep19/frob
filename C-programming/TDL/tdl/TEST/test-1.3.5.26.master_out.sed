Task Control Management x.y.z (MON-DAY-YEAR)
Goal      TaskA-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal      TaskA-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
TaskA
Goal                TaskB {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal                TaskC {3}:        TCM {1} --> ON HOLD         (Inactive)
Goal                TaskB {2}:  ON HOLD  --> TCM             (Sent)
  Success TaskA-auto,wait {1}:
TaskB
Command            TaskE1 {4}:        TCM {2} --> ON HOLD         (Inactive)
Command            TaskE2 {5}:        TCM {2} --> ON HOLD         (Inactive)
Command            TaskE1 {4}:  ON HOLD  --> TCM             (Sent)
  Success           TaskB {2}:
TaskE1
Goal        ourException2 {6}:        TCM {4} --> ON HOLD         (Inactive)
Goal        ourException2 {6}:  ON HOLD  --> TCM             (Sent)
  Failure          TaskE1 {4}:
Excep1Hnd: E1 failed
Goal        ourException2 {7}:        TCM {6} --> ON HOLD         (Inactive)
Goal        ourException2 {7}:  ON HOLD  --> TCM             (Sent)
  Failure   ourException2 {6}:
Excep2Hnd: E1 failed 1
Goal         ourException {8}:        TCM {7} --> ON HOLD         (Inactive)
Goal         ourException {8}:  ON HOLD  --> TCM             (Sent)
  Failure   ourException2 {7}:
Excep3Hnd: E1 failed
  Success    ourException {8}:
Command            TaskE2 {5}:  ON HOLD  --> TCM             (Sent)
TaskE2
  Success          TaskE2 {5}:
Goal                TaskC {3}:  ON HOLD  --> TCM             (Sent)
Command            TaskE3 {9}:        TCM {3} --> ON HOLD         (Inactive)
TaskC
Command            TaskE3 {9}:  ON HOLD  --> TCM             (Sent)
  Success           TaskC {3}:
TaskE3
Goal         ourException {10}:        TCM {9} --> ON HOLD         (Inactive)
Goal         ourException {10}:  ON HOLD  --> TCM             (Sent)
  Failure          TaskE3 {9}:
Excep3Hnd: E3 failed
  Success    ourException {10}:
