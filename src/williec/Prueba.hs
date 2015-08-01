
import qualified Data.Map as Map


main = do
  putStrLn "Prueba"
  let a = Map.insert "variable_name" 0 Map.empty
  let b = Map.toList a
  putStrLn $ show b
