Task Control Management x.y.z (MON-DAY-YEAR)
Goal        run-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal        run-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
run()
Goal                  bar {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal                  bar {2}:  ON HOLD  --> TCM             (Sent)
  Failure   run-auto,wait {1}:
foo()
Goal           resumefooA {3}:        TCM {2} --> ON HOLD         (Inactive)
Goal           resumefooA {3}:  ON HOLD  --> TCM             (Sent)
Goal resumeFooA()
Resume foo(A)
Resume foo(B)
Resume foo(C)
Resume foo(D)
Resume foo(E)
Resume foo(F)
Goal                  bar {4}:        TCM {2} --> ON HOLD         (Inactive)
Goal                  bar {4}:  ON HOLD  --> TCM             (Sent)
  Success             bar {2}:
  Success      resumefooA {3}:
foo(int)
Goal           resumefooG {5}:        TCM {4} --> ON HOLD         (Inactive)
Goal           resumefooG {5}:  ON HOLD  --> TCM             (Sent)
Goal resumeFooG()
Resume foo(G)
Resume foo(H)
Resume foo(I)
Goal                  bar {6}:        TCM {4} --> ON HOLD         (Inactive)
Goal                  bar {6}:  ON HOLD  --> TCM             (Sent)
  Success             bar {4}:
  Success      resumefooG {5}:
foo(double)
Goal           resumefooJ {7}:        TCM {6} --> ON HOLD         (Inactive)
Goal           resumefooJ {7}:  ON HOLD  --> TCM             (Sent)
Goal resumeFooA()
Resume foo(J)
Resume foo(K)
Resume foo(L)


_TDL_ExceptionHandler_foo_test_1_3_5_22_tdl_0::resume():  ERROR:  Expected to find an Action of type "TDL-Exception Handler-foo_test_1_3_5_22_tdl_0".  Instead, found an Action of type "TDL-Exception Handler-foo_test_1_3_5_22_tdl_2".  The cause of this is a TDL_Resume_...() invocation who's leading TCM_Task_Tree_Ref argument corresponds to a TDL Task declaration other than the one that this TDL_Resume...() operation corresponds to.  (There is a 1-N mapping between TDL Goals/Commands/Monitors/ExceptionHandlers and TDL Resume Tasks.)  This is a really *REALLY* _BAD_ thing.  Due to internal casting operations, any task-arguments / persistent task arguments will be severely mangled, along with possible corruption of the internal virtual pointer table for the underlying _Action class.


Resume foo(F)
WARNING: No exception handler found for exception type "bar"
  Success             bar {6}:
  Success      resumefooJ {7}:
