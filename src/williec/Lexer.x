{
-- alex scanner with uulib support.
-- compile: alex -o Lexer.hs -g Lexer.x
module Lexer(tokenize) where

import UU.Scanner
import Data.Word (Word8)
}

-- digits
$digit = 0-9
-- alphabetic characters
$alpha = [a-zA-Z]
-- alphabetic uppercase characters
$alphaupper = [A-Z]

tokens :-

  $white+       ;
  "#".*         ;
  "do"                                     { reserved }
  "read"                                   { reserved }
  "output"                                 { reserved }
  "lift"                                   { reserved }
  "lift2"                                  { reserved }
  "folds"                                  { reserved }
  "{"                                      { reserved }
  "}"                                      { reserved }
  "("                                      { reserved }
  ")"                                      { reserved }
  "="                                      { reserved }
  "<-"                                     { reserved }

  "if"                                     { reserved }
  "then"                                   { reserved }
  "else"                                   { reserved }

  "+"                                      { reserved }
  "-"                                      { reserved }
  "*"                                      { reserved }
  "/"                                      { reserved }
  ">"                                      { reserved }
  "<"                                      { reserved }
  ">="                                     { reserved }
  "<="                                     { reserved }
  "=="                                     { reserved }
  "/="                                     { reserved }
  "and"                                    { reserved }
  "or"                                     { reserved }
  "not"                                    { reserved }
  $digit+                                  { valueToken TkInteger16 }
  $alphaupper [$alphaupper $digit \_]*     { valueToken TkConid }
  $alpha [$alpha $digit \_]*               { valueToken TkVarid }

{
-- Boilerplate code to adapt to uu.scanner
type AlexInput = (Pos, String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar: there is no need to go back in the input."

-- for alex 3 (not compatible with uulib example!)
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (_,[]) = Nothing
alexGetByte (p, (c:cs)) = let p' = adv p c
                          in Just ((fromIntegral $ ord c), (p', cs))

-- This doesnt work with alex 3 (from the uulib example)
alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (_, []) = Nothing
alexGetChar (p, (c:cs))
  = let p' = adv p c
    in Just (c, (p', cs))

-- use Alex scanner to generate a list of tokens for the uulib token parsers
tokenize :: String -> String -> [Token]
tokenize filename str
  = go (initpos, str)
  where
    initpos = Pos 1 1 filename
    
    go inp@(pos, cs)
      = case alexScan inp 0 of
          AlexEOF         -> []
          AlexError inp'  -> valueToken TkError [head cs] pos : go inp'
          AlexSkip inp' _ -> go inp'
          AlexToken inp' len act -> act (take len cs) pos : go inp'
}

