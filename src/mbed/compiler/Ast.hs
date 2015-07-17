
module Ast where

import Data.Foldable

data BinOp = E_Add | E_Sub | E_Mul | E_Div
           | E_GT | E_LT | E_GTe | E_LTe | E_Eq | E_Neq
           | E_And | E_Or
           deriving Show

data UnaryOp = E_Not
              deriving Show

data Expression = E_Value Int
                | E_Var String
                | E_BinExpr BinOp Expression Expression
                | E_UnaryExpr UnaryOp Expression
                | E_If Expression Expression Expression
                deriving Show

data Declaration = E_Fun String [String] Expression
                 | E_Const String Int
                 deriving Show

data FRPApplication = E_Read String String
                    | E_Lift String String String
                    | E_Lift2 String String String String
                    | E_Folds String String Expression String
                    | E_Output String String
                    deriving Show

data WillieAST = E_Root [Declaration] [FRPApplication]
             deriving Show
