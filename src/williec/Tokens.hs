module Tokens where

data Token = T_Const String
           | T_Var String
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
           deriving (Eq, Show)
