
module Ast where

data BinOp = E_Add | E_Sub | E_Mul | E_Div
           deriving Show

data BinComp = E_GT | E_LT | E_GTe | E_LTe | E_Eq | E_Neq
             deriving Show

data BoolExpr = E_Compare BinComp Expression Expression
              | E_And Expression Expression 
              | E_Or Expression Expression
              | E_Not Expression
              deriving Show

data Expression = E_Value Int
                | E_BinExpr BinOp Expression Expression
                | E_Var String
                | E_BoolExpr BoolExpr
                | E_If BoolExpr Expression Expression
                deriving Show

data Declaration = E_Fun [Char] [String] Expression
                 | E_Const String Int
                 deriving Show

data FRPApplication = E_Read String String
                    | E_Lift String String String
                    | E_Lift2 String String String String
                    | E_Folds String String Expression String
                    | E_Output String String
                    deriving Show

data DoDeclaration = E_Do [FRPApplication]
                   deriving Show

data WillieAST = E_Program [Declaration] DoDeclaration
             deriving Show

