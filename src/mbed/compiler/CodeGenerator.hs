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



generate_bytecode :: WillieAST -> WillieBC
generate_bytecode (E_Program decls dodecls) = WillieBC [Thalt]
