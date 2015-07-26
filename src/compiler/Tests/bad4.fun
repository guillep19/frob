church0 = \f -> \x -> x ;
churchS = \n -> \f -> \x -> f (n f x) ;
church2int n = n (\i -> i + 1) 0 ;
int2church i = if (i < 1) then church0 else churchS (int2church (i-1)) ;
main = int2church 8 ;  
-- the result is a function or closure; this can be printed as an expression,
-- but should be reported as a run-time type error because main must return an
-- integer according to PM




