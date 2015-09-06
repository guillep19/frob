module CodeGenerator (gen_bc) where

import Ast
import Bytecode
import Env

gen_bc_unary_op :: UnaryOp -> Env -> (Env, WillieBC)
gen_bc_unary_op E_Not e = (e, [Top_not])

gen_bc_bin_op :: BinOp -> Env -> (Env, WillieBC)
gen_bc_bin_op E_Add e = (e, [Tadd])
gen_bc_bin_op E_Sub e = (e, [Tsub])
gen_bc_bin_op E_Mul e = (e, [Tmul])
gen_bc_bin_op E_Div e = (e, [Tdiv])
gen_bc_bin_op E_GT e = (e, [Tcmp_gt])
gen_bc_bin_op E_LT e = (e, [Tcmp_lt])
gen_bc_bin_op E_GTe e = (e, [Tcmp_lt, Top_not])
gen_bc_bin_op E_LTe e = (e, [Tcmp_gt, Top_not])
gen_bc_bin_op E_Eq e = (e, [Tcmp_eq])
gen_bc_bin_op E_Neq e = (e, [Tcmp_neq])
gen_bc_bin_op E_And e = (e, [Top_and])
gen_bc_bin_op E_Or e = (e, [Top_or])

gen_bc_expression :: Expression -> Env -> (Env, WillieBC)
gen_bc_expression (E_Value value) e = (e, [Tpush value])
gen_bc_expression (E_BinExpr bin_op expr1 expr2) e =
    let (_, a) = gen_bc_bin_op bin_op e
        (e1, a1) = gen_bc_expression expr1 e
        (e2, a2) = gen_bc_expression expr2 e1 in
    (e2, a1 ++ a2 ++ a)
gen_bc_expression (E_Call expr args) env =
    let (env1, bc) = gen_bc_expression expr env in
    (env1, bc ++ [Tcall 1242])
gen_bc_expression _ env = (env, [Tdup])

gen_bc_decl :: Declaration -> WillieBC
gen_bc_decl (E_Const name value) =
    let index = 1919 in
    [Tpush value, Tstore index]
gen_bc_decl (E_Fun name params expr) = [Tlabel name]

-- This can be a fold
gen_bc_decls :: [Declaration] -> WillieBC
gen_bc_decls (x:xs) = let ax = gen_bc_decl x
                          axs = gen_bc_decls xs in
                      ax ++ axs
gen_bc_decls [] = []

gen_bc_dodecl :: FRPApplication -> Env -> (Env, WillieBC)
gen_bc_dodecl (E_Read id expr) env =
    let (env1, a) = gen_bc_expression expr env
        env2 = add_frp_id id env1
        index = find_frp_id id env2 in
    (env2, a ++ [Tread index])
gen_bc_dodecl (E_Lift id src fun) env =
    let source = find_frp_id src env
        env2 = add_frp_id id env
        index = find_frp_id id env2 in
    (env2, [Tlift index source fun])
gen_bc_dodecl (E_Lift2 id src1 src2 fun) env =
    let source1 = find_frp_id src1 env
        source2 = find_frp_id src2 env
        env1 = add_frp_id id env
        index = find_frp_id id env1 in
    (env1, [Tlift2 index source1 source2 fun])
gen_bc_dodecl (E_Folds id src fun expr) env =
    let (env1, a) = gen_bc_expression expr env
        source = find_frp_id src env1
        env2 = add_frp_id id env1
        index = find_frp_id id env2 in
    (env2, a ++ [Tfolds index source fun])
gen_bc_dodecl (E_Output src expr) env =
    let (env1, a) = gen_bc_expression expr env
        source = find_frp_id src env1 in
    (env1, a ++ [Twrite source])

gen_bc_dodecls :: [FRPApplication] -> Env -> (Env, WillieBC)
gen_bc_dodecls (x:xs) env = let (env1, a) = gen_bc_dodecl x env
                                (env2, b) = gen_bc_dodecls xs env1 in
                          (env2, a ++ b)
gen_bc_dodecls [] env = (env, [])

gen_bc :: WillieAST -> WillieBC
gen_bc prog = let env = emptyEnv
                  (env1, ado) = gen_bc_dodecls (frpcode prog) env
                  (adecls) = gen_bc_decls (decls prog) in
            ado ++ [Thalt] -- ++ adecls
