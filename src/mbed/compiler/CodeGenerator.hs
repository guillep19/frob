module CodeGenerator (generate_bytecode) where

import Ast
import Bytecode
import qualified Data.Map as Map

data Env = E_Fruta (Map.Map String Int) Int
         deriving Show

exists_var :: String -> Env -> Bool
exists_var name (E_Fruta m nextid) = Map.member name m

add_vardecl :: String -> Env -> Env
add_vardecl name (E_Fruta m nextid) = if (exists_var name (E_Fruta m nextid)) then (E_Fruta (Map.insert name (nextid) m) (nextid+1)) else (E_Fruta m nextid)

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

generate_bytecode_dodecls :: [FRPApplication] -> WillieBC
generate_bytecode_dodecls (x:xs) = WillieBC [] []
generate_bytecode_dodecls [] = WillieBC [] []

generate_bytecode :: WillieAST -> WillieBC
generate_bytecode (E_Root decls do_decls) = let WillieBC ado bdo = generate_bytecode_dodecls do_decls
                                                WillieBC adecls bdecls = generate_bytecode_decls decls in
                                                      WillieBC (ado ++ [Thalt] ++ adecls) (bdo ++ bdecls)
