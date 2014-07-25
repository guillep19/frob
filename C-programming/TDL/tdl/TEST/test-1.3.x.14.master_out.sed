Task Control Management x.y.z (MON-DAY-YEAR)
 f
Goal           foo-from-f {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal       test-auto,wait {2}:        TCM {0} --> ON HOLD         (Inactive)
Goal           foo-from-f {1}:  ON HOLD  --> TCM             (Sent)
Goal       test-auto,wait {2}:  ON HOLD  --> TCM             (Sent)
 foo
  Success      foo-from-f {1}:
 test
Goal                  bar {3}:        TCM {2} --> ON HOLD         (Inactive)
Goal                  bar {3}:  ON HOLD  --> TCM             (Sent)
  Success  test-auto,wait {2}:
 bar
Goal         foo-from-bar {4}:        TCM {3} --> ON HOLD         (Inactive)
Goal         foo-from-bar {4}:  ON HOLD  --> TCM             (Sent)
  Success             bar {3}:
 foo
  Success    foo-from-bar {4}:
