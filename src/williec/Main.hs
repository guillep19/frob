module Main(main) where

import UU.Parsing
import UU.Scanner.Token
import UU.Scanner.TokenParser
import UU.Scanner.GenTokenOrd()
import UU.Scanner.GenTokenSymbol()
import UU.Scanner.TokenShow()
import UU.Scanner.Position
import Data.List
import UU.Pretty
import System.Environment
import System.IO
import Data.Char

import Lexer
import Parser
import AttributeGrammar

main :: IO ()
main
  = do args <- getArgs
       if (length args /= 2)
        then putStrLn "usage: williec <source> <dest>"
        else let [source,dest] = args
             in compile source dest

compile :: String -> String -> IO ()
compile source dest
  = do input <- readFile source
       --let toks = runScanner source input
       let toks = alexScanTokens input
       sem <- runParser toks
       let output = transform $ sem
       writeFile dest output

runScanner :: String -> String -> [Token]
runScanner filename
  = scanBlock (initPos filename)

runParser :: [Token] -> IO T_Root
runParser
  = parseIOMessage show pRoot

transform :: T_Root -> String
transform sem
  = let inh = Inh_Root {}
        syn = wrap_Root sem inh
    in code_Syn_Root syn
