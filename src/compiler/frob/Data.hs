module Data () where

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

data Fruit = Afruit
