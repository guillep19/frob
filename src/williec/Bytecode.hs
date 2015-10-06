
module Bytecode where

import Data.Binary
import Data.Bits

data OpCode = Thalt
            | Tcall {fun :: Int}
            | Tret
            | Tload_param {id :: Int}
            | Tlift {sid :: Int, source :: Int, fun :: Int}
            | Tlift2 {sid :: Int, source1 :: Int, source2 :: Int, fun :: Int}
            | Tfolds {sid :: Int, source :: Int, fun :: Int}
            | Tread {sid :: Int}
            | Twrite {sid :: Int}
            | Tjump Int
            | Tjump_false Int
            | Tcmp_eq
            | Tcmp_neq
            | Tcmp_gt
            | Tcmp_gte
            | Tcmp_lt
            | Tcmp_lte
            | Tadd
            | Tsub
            | Tdiv
            | Tmul
            | Top_and
            | Top_or
            | Top_not
            | Tpush {value :: Int}
            | Tpop
            | Tdup
            | Tstore Int
            | Tload Int
            deriving (Eq, Ord, Show, Read)

type BC = [OpCode]

instrLen :: OpCode -> Int
instrLen (Tcall word) = 2
instrLen (Tlift inm src fun) = 3
instrLen (Tlift2 inm src1 src2 fun) = 4
instrLen (Tfolds inm src1 fun) = 3
instrLen (Tjump word) = 2
instrLen (Tjump_false word) = 2
instrLen (Tpush word) = 2
instrLen _ = 1

printOpcode :: OpCode -> [String]
printOpcode (Thalt) = ["t_halt"]
printOpcode (Tcall fun) = ["t_call", show(fun)]
printOpcode (Tret) = ["t_ret"]
printOpcode (Tload_param id) = ["t_load_param " ++ show(id)]
printOpcode (Tlift sid source fun) = ["t_lift " ++ show(sid),
                                      show(source),
                                      show(fun)]
printOpcode (Tlift2 sid source1 source2 fun) = ["t_lift2 " ++ show(sid),
                                                show(source1),
                                                show(source2),
                                                show(fun)]
printOpcode (Tfolds sid source fun) = ["t_folds " ++ show(sid),
                                       show(source),
                                       show(fun)]
printOpcode (Tread sid) = ["t_read " ++ show(sid)]
printOpcode (Twrite sid) = ["t_write " ++ show(sid)]
printOpcode (Tjump pos) = ["t_jump", show(pos)]
printOpcode (Tjump_false pos) = ["t_jump_false", show(pos)]
printOpcode (Tcmp_eq) = ["t_cmp_eq"]
printOpcode (Tcmp_neq) = ["t_cmp_neq"]
printOpcode (Tcmp_gt) = ["t_cmp_gt"]
printOpcode (Tcmp_gte) = ["t_cmp_gte"]
printOpcode (Tcmp_lt) = ["t_cmp_lt"]
printOpcode (Tcmp_lte) = ["t_cmp_lte"]
printOpcode (Tadd) = ["t_add"]
printOpcode (Tsub) = ["t_sub"]
printOpcode (Tdiv) = ["t_div"]
printOpcode (Tmul) = ["t_mul"]
printOpcode (Top_and) = ["t_op_and"]
printOpcode (Top_or) = ["t_op_or"]
printOpcode (Top_not) = ["t_op_not"]
printOpcode (Tpush value) = ["t_push", show(value)]
printOpcode (Tpop) = ["t_pop"]
printOpcode (Tdup) = ["t_dup"]
printOpcode (Tstore pos) = ["t_store " ++ show(pos)]
printOpcode (Tload pos) = ["t_load " ++ show(pos)]


printBC :: BC -> String
printBC bc = let lines = (foldr (++) [] (map printOpcode bc))
                 lineNumbers = map (\n -> show(n) ++ ": ") [0..] in
             foldr (++) "" (map (\(x,y) -> x ++ y ++ "\n") (zip lineNumbers lines))




printBinaryOpcode :: OpCode -> [Word16]
printBinaryOpcode (Thalt) = [0x0000]
printBinaryOpcode (Tcall fun) = [0x0100, (fromIntegral fun :: Word16)]
printBinaryOpcode (Tret) = [0x0200]
printBinaryOpcode (Tload_param id) = [0x0300 + (0x00ff .&. fromIntegral id :: Word16)]
printBinaryOpcode (Tlift sid source fun) = [0x0400 .|. (0x00ff .&. fromIntegral sid :: Word16),
                                            (fromIntegral source :: Word16),
                                            (fromIntegral fun :: Word16)]
printBinaryOpcode (Tlift2 sid source1 source2 fun) 
   = [0x0500 .|. (0x00ff .&. fromIntegral sid :: Word16),
     (fromIntegral source1 :: Word16),
     (fromIntegral source2 :: Word16),
     (fromIntegral fun :: Word16)]
printBinaryOpcode (Tfolds sid source fun) = [0x0600 .|. (0x00ff .&. fromIntegral sid :: Word16),
                                             (fromIntegral source :: Word16),
                                             (fromIntegral fun :: Word16)]
printBinaryOpcode (Tread sid) = [0x0700 .|. (0x00ff .&. fromIntegral sid :: Word16)]
printBinaryOpcode (Twrite sid) = [0x0800 .|. (0x00ff .&. fromIntegral sid :: Word16)]
printBinaryOpcode (Tjump pos) = [0x0900, (fromIntegral pos :: Word16)]
printBinaryOpcode (Tjump_false pos) = [0x0a00, (fromIntegral pos :: Word16)]
printBinaryOpcode (Tcmp_eq) = [0x0b00]
printBinaryOpcode (Tcmp_neq) = [0x0c00]
printBinaryOpcode (Tcmp_gt) = [0x0d00]
printBinaryOpcode (Tcmp_gte) = [0x0d00] -- mock value (unimplemented in vm)
printBinaryOpcode (Tcmp_lt) = [0x0e00] 
printBinaryOpcode (Tcmp_lte) = [0x0e00] -- mock value (unimplemented in vm)
printBinaryOpcode (Tadd) = [0x0f00]
printBinaryOpcode (Tsub) = [0x1000]
printBinaryOpcode (Tdiv) = [0x1100]
printBinaryOpcode (Tmul) = [0x1200]
printBinaryOpcode (Top_and) = [0x1300]
printBinaryOpcode (Top_or) = [0x1400]
printBinaryOpcode (Top_not) = [0x1500]
printBinaryOpcode (Tpush value) = [0x1600, (fromIntegral value :: Word16)]
printBinaryOpcode (Tpop) = [0x1700]
printBinaryOpcode (Tdup) = [0x1800]
printBinaryOpcode (Tstore pos) = [0x1900 .|. (0x00ff .&. fromIntegral pos :: Word16)]
printBinaryOpcode (Tload pos) = [0x1a00 .|. (0x00ff .&. fromIntegral pos :: Word16)]

