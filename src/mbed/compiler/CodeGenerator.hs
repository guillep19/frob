module CodeGenerator (generate_bytecode) where

import Ast
import Bytecode
import qualified Data.Map as Map

data Env = E_Env (Map.Map String Int) Int (Map.Map String Int) Int
         deriving Show

exists_var :: String -> Env -> Bool
exists_var name (E_Env m nextid i1 i2) = Map.member name m

add_vardecl :: String -> Env -> Env
add_vardecl name (E_Env m nextid i1 i2) = if (exists_var name (E_Env m nextid i1 i2))
                                        then (E_Env (Map.insert name (nextid) m) (nextid+1) i1 i2)
                                        else (E_Env m nextid i1 i2)

add_frp_id :: String -> Env -> Env
add_frp_id id env = env

find_frp_id :: String -> Env -> Int
find_frp_id id env = 19

generate_bytecode_bin_op :: BinOp -> Env -> (Env, WillieBC)
generate_bytecode_bin_op E_Add e = (e, WillieBC [Tadd] [])
generate_bytecode_bin_op E_Sub e = (e, WillieBC [Tsub] [])
generate_bytecode_bin_op E_Mul e = (e, WillieBC [Tmul] [])
generate_bytecode_bin_op E_Div e = (e, WillieBC [Tdiv] [])

generate_bytecode_expression :: Expression -> Env -> (Env, WillieBC)
generate_bytecode_expression (E_Value value) e = (e, WillieBC [Tpush value] [])
generate_bytecode_expression (E_BinExpr bin_op expr1 expr2) e =
                                  let (_, WillieBC a b) = generate_bytecode_bin_op bin_op e
                                      (e1, WillieBC a1 b1) = generate_bytecode_expression expr1 e
                                      (e2, WillieBC a2 b2) = generate_bytecode_expression expr2 e1 in
                                  (e2, WillieBC (a1 ++ a2 ++ a) (b ++ b1 ++ b2))

generate_bytecode_decl :: Declaration -> WillieBC
generate_bytecode_decl (E_Const name value) = WillieBC [Tpush value, Tstore 1] []
generate_bytecode_decl decl = WillieBC [Tdiv] []

generate_bytecode_decls :: [Declaration] -> WillieBC
generate_bytecode_decls (x:xs) = let WillieBC ax bx = generate_bytecode_decl x
                                     WillieBC axs bxs = generate_bytecode_decls xs in
                                 WillieBC (ax ++ axs) (bx ++ bxs)
generate_bytecode_decls [] = WillieBC [] []

generate_bytecode_dodecl :: FRPApplication -> Env -> (Env, WillieBC)
generate_bytecode_dodecl (E_Read id expr) env = let (env1, WillieBC a b) = generate_bytecode_expression expr env
                                                    env2 = add_frp_id id env1
                                                    index = find_frp_id id env2 in
                                                (env2, WillieBC (a ++ [Tread index]) [])
generate_bytecode_dodecl (E_Lift id src fun) env = let source = find_frp_id src env
                                                       env2 = add_frp_id id env
                                                       index = find_frp_id id env2 in
                                                   (env2, WillieBC [Tlift index source fun] [])
generate_bytecode_dodecl (E_Lift2 id src1 src2 fun) env = let source1 = find_frp_id src1 env
                                                              source2 = find_frp_id src2 env
                                                              env1 = add_frp_id id env
                                                              index = find_frp_id id env1 in
                                                        (env1, WillieBC [Tlift2 index source1 source2 fun] [])
generate_bytecode_dodecl (E_Folds id src fun expr) env = let (env1, WillieBC a _) = generate_bytecode_expression expr env
                                                             source = find_frp_id src env1
                                                             env2 = add_frp_id id env1
                                                             index = find_frp_id id env2 in
                                                       (env2, WillieBC (a ++ [Tfolds index source fun]) [])
generate_bytecode_dodecl (E_Output src expr) env = let (env1, WillieBC a _) = generate_bytecode_expression expr env
                                                       source = find_frp_id src env1 in
                                                 (env1, WillieBC (a ++ [Tread source]) [])

generate_bytecode_dodecls :: [FRPApplication] -> Env -> (Env, WillieBC)
generate_bytecode_dodecls (x:xs) env = let (env1, WillieBC a _) = generate_bytecode_dodecl x env
                                           (env2, WillieBC b _) = generate_bytecode_dodecls xs env1 in
                                      (env2, WillieBC (a ++ b) [])
generate_bytecode_dodecls [] env = (env, WillieBC [] [])

generate_bytecode :: WillieAST -> WillieBC
generate_bytecode (E_Root decls do_decls) = let env = (E_Env (Map.empty) 0 (Map.empty) 0)
                                                (WillieBC adecls bdecls) = generate_bytecode_decls decls
                                                (env1, WillieBC ado bdo) = generate_bytecode_dodecls do_decls env in
                                                      WillieBC (ado ++ [Thalt] ++ adecls) (bdo ++ bdecls)
