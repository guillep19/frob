Task Control Management x.y.z (MON-DAY-YEAR)
Monitor     foo-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Monitor     foo-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
Command   ACT-foo-auto,wait {2}:        TCM {1} --> ON HOLD         (Inactive)
Command   ACT-foo-auto,wait {2}:  ON HOLD  --> TCM             (Sent)
Test-Monitor: foo ( one )
  Success ACT-foo-auto,wait {2}:
Command   ACT-foo-auto,wait {4}:        TCM {1} --> ON HOLD         (Inactive)
Command   ACT-foo-auto,wait {4}:  ON HOLD  --> TCM             (Sent)
Test-Monitor: foo ( one )
  Success ACT-foo-auto,wait {4}:
Command   ACT-foo-auto,wait {5}:        TCM {1} --> ON HOLD         (Inactive)
Command   ACT-foo-auto,wait {5}:  ON HOLD  --> TCM             (Sent)
Test-Monitor: foo ( one )
  Success ACT-foo-auto,wait {5}:
Command   ACT-foo-auto,wait {6}:        TCM {1} --> ON HOLD         (Inactive)
Command   ACT-foo-auto,wait {6}:  ON HOLD  --> TCM             (Sent)
Test-Monitor: foo ( one )
  Success ACT-foo-auto,wait {6}:
Command   ACT-foo-auto,wait {7}:        TCM {1} --> ON HOLD         (Inactive)
Command   ACT-foo-auto,wait {7}:  ON HOLD  --> TCM             (Sent)
 Complete   foo-auto,wait {1}:
Test-Monitor: foo ( one )
  Success ACT-foo-auto,wait {7}:
