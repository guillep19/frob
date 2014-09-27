{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+       ;
  "--".*        ;
  "|="                             { \s -> Constraint_I }
  "->"                             { \s -> Constraint_Arrow }
  Function                         { \s -> Function }
  Return                           { \s -> Return }
  Task                             { \s -> Task }
  Observable                       { \s -> Observable }
  Loop                             { \s -> Loop }
  If                               { \s -> If }
  Else                             { \s -> Else }
  Not                              { \s -> Not }
  And                              { \s -> And }
  Or                               { \s -> Or }
  False                            { \s -> TFalse }
  True                             { \s -> TTrue }
  Start                            { \s -> Start }
  $digit+ \. $digit*               { \s -> Float (read s) }
  $digit+                          { \s -> Int (read s) }
  [\=\+\-\*\,\{\}\(\)\<\>\:\[\]\.] { \s -> Sym (head s) }
  $alpha [$alpha $digit \_]*       { \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Function         |
  Return           |
  Task             |
  Constraint_I     |
  Constraint_Arrow |
  Observable       | 
  Loop             | 
  If               | 
  Else             | 
  Not              | 
  And              | 
  Or               | 
  TFalse           | 
  TTrue            | 
  Start            |
  Sym Char         |
  Var String       |
  Float Float      |
  Int Int     
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
