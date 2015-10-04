
module Env where
import qualified Data.Map as Map
import Data.List

data Env = E_Env {
           frps :: [String],
           scope :: [[String]],
           labels :: 
         } deriving Show

emptyEnv :: Env
emptyEnv = (E_Env [] [[]])

addFrpId :: String -> Env -> Env
addFrpId id env = env { frps = id:(frps env) }
                  where count = length (frps env)

findFrpId :: String -> Env -> Int
findFrpId id env = case elemIndex id (frps env) of
                   Just n -> n
                   Nothing -> error ("Frp id=" ++ id ++ " does not exist.")




type LabelMap = Map.Map String Int

emptyLabelMap :: LabelMap
emptyLabelMap = Map.empty

addLabel :: String -> Int -> LabelMap -> LabelMap
addLabel label pos labels = Map.insert label pos labels

getLabel :: String -> LabelMap -> Int
getLabel label labels = case Map.lookup label labels of
                        Just n -> n
                        Nothing -> error ("Function not defined: " ++ label)
