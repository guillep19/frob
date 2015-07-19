
module Env where
import qualified Data.Map as Map

data Env = E_Env { vars :: (Map.Map String Int),
                   varcount :: Int,
                   frps :: (Map.Map String Int),
                   frpcount :: Int,
                   labels :: (Map.Map String Int)
         } deriving Show

emptyEnv :: Env
emptyEnv = (E_Env Map.empty 0 Map.empty 0 Map.empty)

exists_var :: String -> Env -> Bool
exists_var name = Map.member name . vars

add_vardecl :: String -> Env -> Env
add_vardecl name env = if (Map.member name $ vars env)
            then env
            else env { varcount = (varcount env) + 1 }

lookup_var :: String -> Env -> Int
lookup_var str env = case Map.lookup str (vars env) of
                   Just n -> n
                   Nothing -> 1998

add_frp_id :: String -> Env -> Env
add_frp_id id env = env { frps = Map.insert id count (frps env),
                          frpcount = count + 1 }
                  where count = frpcount env

find_frp_id :: String -> Env -> Int
find_frp_id id env = case Map.lookup id (frps env) of
                   Just n -> n
                   Nothing -> 9999

set_label_index :: String -> Int -> Env -> Env
set_label_index label index env = env

get_label_index :: String -> Env -> Int
get_label_index label env = 100000
