{
  module Lexer where

  import Tokens
}

%wrapper "basic"

-- digits
$digit = 0-9
-- alphabetic characters
$alpha = [a-zA-Z]
-- alphabetic uppercase characters
$alphaupper = [A-Z]

tokens :-

  $white+       ;
  "#".*         ;
  $alphaupper [$alphaupper $digit \_]*     { \s -> T_Const s }
  $alpha [$alpha $digit \_]*               { \s -> T_Var s }
  $digit+                                  { \s -> Int (read s) }
  Do                                       { \s -> T_Do }
  Read                                     { \s -> T_Read }
  Output                                   { \s -> T_Output }
  Lift                                     { \s -> T_Lift }
  Lift2                                    { \s -> T_Lift2 }

  "{"                                      { \s -> T_LCurly }
  "}"                                      { \s -> T_RCurly }
  "("                                      { \s -> T_LParen }
  ")"                                      { \s -> T_RParen }
  "="                                      { \s -> T_Equal }
  "<-"                                     { \s -> T_Arrow }

  If                                       { \s -> T_If }
  Then                                     { \s -> T_Then }
  Else                                     { \s -> T_Else }

  "+"                                      { \s -> T_Add }
  "-"                                      { \s -> T_Sub }
  "*"                                      { \s -> T_Mul }
  "/"                                      { \s -> T_Div }
  ">"                                      { \s -> T_GT }
  "<"                                      { \s -> T_LT }
  "=>"                                     { \s -> T_GTe }
  "<="                                     { \s -> T_LTe }
  "=="                                     { \s -> T_Eq }
  "/="                                     { \s -> T_Neq }
  And                                      { \s -> T_And }
  Or                                       { \s -> T_Or }
  Not                                      { \s -> T_Not }
