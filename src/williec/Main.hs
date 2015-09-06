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
import Html

main :: IO ()
main
  = do args <- getArgs
       if (length args /= 2)
        then putStrLn "usage: html <source> <dest>"
        else let [source,dest] = args
             in compile source dest

compile :: String -> String -> IO ()
compile source dest
  = do input  <- readFile source
       let toks = runScanner source input
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
    in html_Syn_Root syn

augment :: String -> String
augment doc
  = "<html lang=\"en\"><head><title>Generated HTML document</title></head><body>\n" ++ doc ++ "\n</body></html>"

