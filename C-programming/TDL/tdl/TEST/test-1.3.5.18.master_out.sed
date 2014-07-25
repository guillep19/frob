Task Control Management x.y.z (MON-DAY-YEAR)
Goal          A-auto,wait {1}:        TCM {0} --> ON HOLD         (Inactive)
Goal          A-auto,wait {1}:  ON HOLD  --> TCM             (Sent)
Goal A: Root Node
Goal                    B {2}:        TCM {1} --> ON HOLD         (Inactive)
Goal          E-auto,wait {3}:        TCM {1} --> ON HOLD         (Inactive)
Goal                    B {2}:  ON HOLD  --> TCM             (Sent)
Goal          E-auto,wait {3}:  ON HOLD  --> TCM             (Sent)
Goal B: A-auto,wait
Goal                    C {4}:        TCM {2} --> ON HOLD         (Inactive)
Goal                    C {4}:  ON HOLD  --> TCM             (Sent)
Goal E: A-auto,wait
  Success     E-auto,wait {3}:
  Success     A-auto,wait {1}:
Goal C: B
Resume B: A-auto,wait
Resume B:  UserTaskForThreadStack:  0x........ (B)   
Goal                    D {5}:        TCM {2} --> ON HOLD         (Inactive)
Goal          E-auto,wait {6}:        TCM {2} --> ON HOLD         (Inactive)
Goal                    D {5}:  ON HOLD  --> TCM             (Sent)
Goal          E-auto,wait {6}:  ON HOLD  --> TCM             (Sent)
Goal D: B
Goal D:  UserTaskForThreadStack:  (nil) (nil)   
Goal D:  UserTaskForThreadStack:  0x........ (B)   
Goal          F-auto,wait {7}:        TCM {5} --> ON HOLD         (Inactive)
Goal          F-auto,wait {7}:  ON HOLD  --> TCM             (Sent)
Goal E: B
Goal E:  UserTaskForThreadStack:  (nil) (nil)   
Goal E:  UserTaskForThreadStack:  0x........ (B)   
  Success     E-auto,wait {6}:
Goal F: D
Goal F:  UserTaskForThreadStack:  (nil) (nil)   
Goal F:  UserTaskForThreadStack:  0x........ (B)   
  Success     F-auto,wait {7}:
Goal                    F {8}:        TCM {5} --> ON HOLD         (Inactive)
_TDL_Wait ( Constrain=F-0 )
Constraint:  _TDL_Wait  (0x........)

Goal                    F {8}:  ON HOLD  --> TCM             (Sent)
Goal F: D
Goal F:  UserTaskForThreadStack:  (nil) (nil)   
Goal F:  UserTaskForThreadStack:  0x........ (B)   
  Success               F {8}:
Goal                    F {9}:        TCM {5} --> ON HOLD         (Inactive)
Root Node {0} [uh|ag|pg|al]
   A-auto,wait {1} [hd|ag|pg|al]
      B {2} [hg|ag|pg|al]
         C {4} [hg|ag|pg|al]
         D {5} [hg|ag|pg|al]
            F-auto,wait {7} [hd|ad|pd|al]
            F {8} [hd|ad|pd|al]
            F {9} [uh|ua|up|al]
         E-auto,wait {6} [hd|ad|pd|al]
      E-auto,wait {3} [hd|ad|pd|al]
Goal                    F {9}:  ON HOLD  --> TCM             (Sent)
  Success               D {5}:
  Success               B {2}:
  Success               C {4}:
Goal F: D
  Success               F {9}:
