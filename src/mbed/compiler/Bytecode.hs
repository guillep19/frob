
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
            deriving (Eq, Ord, Show, Read)

data LabelMap = Label_Map String Int
              | Var_Map String Int
              deriving Show

data WillieBC = WillieBC [OpCode] [LabelMap]
              deriving Show
