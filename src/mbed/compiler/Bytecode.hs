
module Bytecode where

data OpCode = Thalt
            | Tcall Int
            | Tret
            | Tload_param Int
            | Tlift {id :: Int, source :: Int, fun :: String}
            | Tlift2 {id :: Int, source1 :: Int,
                      source2 :: Int, fun :: String}
            | Tfolds {id :: Int, source :: Int, fun :: String}
            | Tread Int
            | Twrite Int
            | Tjump Int
            | Tjump_false Int
            | Tcmp_eq
            | Tcmp_neq
            | Tcmp_gt
            | Tcmp_lt
            | Tadd
            | Tsub
            | Tdiv
            | Tmul
            | Top_and
            | Top_or
            | Top_not
            | Tpush Int
            | Tpop
            | Tdup
            | Tstore Int
            | Tload Int
            | Tlabel String
            deriving (Eq, Ord, Show, Read)

type WillieBC = [OpCode]

instr_length :: OpCode -> Int
instr_length (Tcall word) = 2
instr_length (Tlift inm src fun) = 3
instr_length (Tlift2 inm src1 src2 fun) = 4
instr_length (Tfolds inm src1 fun) = 3
instr_length (Tjump word) = 2
instr_length (Tjump_false word) = 2
instr_length (Tpush word) = 2
instr_length _ = 1


