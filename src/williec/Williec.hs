
module Main where

import Bytecode
import AttributeGrammar

program :: Root
program = Root_Root (Decls_Nil)

main :: IO ()
main = do
  putStrLn "Alf? Willie? Alf? Willie? Alf! Willie!"
  putStrLn $ show program
  putStrLn "Compilando... Alf? Willie!"
  putStrLn "-----------------------------"
