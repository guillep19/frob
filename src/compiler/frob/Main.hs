
module Main (main) where
import Tokens

main = do
  s <- getContents
  print (alexScanTokens s)

