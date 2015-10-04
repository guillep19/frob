
module Bytecode where

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


