mult x y = 
    if (y < 1) then 0 else if (y < 2) then x else (x + (mult x (y-1))) ; 
fact = \x -> if (x < 3) then x else mult x (fact (x-1)) ;
mainz = fact 6 ;
-- no main
