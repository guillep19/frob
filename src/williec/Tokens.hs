module Tokens where

data Token = T_Identifier --Used to match in grammar
           | T_Constant --Used to match in grammar
           | T_Const { constname :: String }
           | T_Ident { varname :: String }
           | T_Int Int
           | T_Do | T_Read | T_Output | T_Lift | T_Lift2 | T_Folds
           | T_LCurly
           | T_RCurly
           | T_LParen
           | T_RParen
           | T_Equal
           | T_Arrow
           | T_If | T_Then | T_Else
           | T_Add | T_Sub | T_Mul | T_Div
           | T_GT | T_LT | T_GTe | T_LTe | T_Eq | T_Neq
           | T_And | T_Or | T_Not
           deriving (Show)

instance Eq Token where
  (T_Ident _) == T_Identifier = True
  (T_Const _) == T_Constant = True
