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
       let toks = tokenize source input
       putStrLn "-- Tokens: --"
       putStrLn (show toks)
       putStrLn "-----------------"
       root <- runParser toks
       putStrLn "-- AST: --"
       putStrLn (show root)
       putStrLn "-----------------"
       let output = transform root
       putStrLn "-- Código: --"
       putStrLn output
       putStrLn "-----------------"
       writeFile dest output

runParser :: [Token] -> IO Root
runParser = parseIOMessage show pRoot
--runParser t = parseIOMessage show pRoot $ t

transform :: Root -> String
transform r
  = code_Syn_Root syn
  where inh = Inh_Root {}
        syn = wrap_Root (sem_Root r) inh
