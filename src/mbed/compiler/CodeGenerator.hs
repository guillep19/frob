module CodeGenerator (generate_bytecode) where

import Ast
import Bytecode


instr_length :: OpCode -> Int
instr_length Thalt = 2
instr_length (Tcall _) = 2
instr_length Tret = 1
instr_length (Tload_param _) = 1
instr_length (Tlift _ _ _) = 3
instr_length (Tlift2 _ _ _ _) = 4
instr_length (Tfolds _ _ _) = 3
instr_length (Tread _ _) = 2
instr_length (Twrite _ _) = 2
instr_length (Tjump _) = 2
instr_length (Tjump_false _) = 2
instr_length (Tcmp_eq) = 1
instr_length (Tcmp_neq) = 1
instr_length (Tcmp_gt) = 1
instr_length (Tcmp_lt) = 1
instr_length (Tadd) = 1
instr_length (Tsub) = 1
instr_length (Tdiv) = 1
instr_length (Tmul) = 1
instr_length (Top_and) = 1
instr_length (Top_or) = 1
instr_length (Top_not) = 1
instr_length (Tpush _) = 2
instr_length (Tpop) = 1
instr_length (Tdup) = 1
instr_length (Tstore _) = 1
instr_length (Tload _) = 1

generate_bytecode_bin_op :: BinOp -> WillieBC
generate_bytecode_bin_op E_Add = WillieBC [Tadd] []
generate_bytecode_bin_op E_Sub = WillieBC [Tsub] []
generate_bytecode_bin_op E_Mul = WillieBC [Tmul] []
generate_bytecode_bin_op E_Div = WillieBC [Tdiv] []

generate_bytecode_expression :: Expression -> WillieBC
generate_bytecode_expression (E_Value value) = WillieBC [Tpush value] []
generate_bytecode_expression (E_BinExpr bin_op expr1 expr2) =
                                  let WillieBC a b = generate_bytecode_bin_op bin_op
                                      WillieBC a1 b1 = generate_bytecode_expression expr1
                                      WillieBC a2 b2 = generate_bytecode_expression expr2 in
                                  WillieBC (a1 ++ a2 ++ a) (b ++ b1 ++ b2)

generate_bytecode_decl :: Declaration -> WillieBC
generate_bytecode_decl (E_Const name value) = let index = create_index name
                                              WillieBC [Tpush value, Tstore name] []
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
generate_bytecode (E_Program decls (E_Do do_decls)) = let WillieBC ado bdo = generate_bytecode_dodecls do_decls
                                                          WillieBC adecls bdecls = generate_bytecode_decls decls in
                                                      WillieBC (ado ++ [Thalt] ++ adecls) (bdo ++ bdecls)
