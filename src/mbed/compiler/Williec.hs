
module Main where

import Ast
import Bytecode
import CodeGenerator

program :: WillieAST
program = E_Program [
            E_Const "INPUT_DISTANCE" 1,
            E_Const "INPUT_COLOR_LEFT" 2,
            E_Const "INPUT_COLOR_RIGHT" 3,
            E_Const "OUTPUT_ENGINE_LEFT" 1,
            E_Const "OUTPUT_ENGINE_RIGHT" 2,
            E_Const "MIN_DISTANCE" 100,
            E_Const "MIN_GREY" 50,
            E_Fun "hay_casa" ["d"]
            (
              E_If
                (E_Compare E_LT (E_Var "d") (E_Var "MIN_DISTANCE"))
                (E_Value 1)
                (E_Value 0)
            ),
            E_Fun "distinto" ["a", "b"]
            (
              E_If
                (E_Compare E_Neq (E_Var "a") (E_Var "b"))
                (E_Value 1)
                (E_Value 0)
            ),
            E_Fun "velocidad_casa" ["num"]
            (
              E_If
                (E_Compare E_GTe (E_Var "num") (E_Value 5))
                (E_Value 0)
                (E_Value 100)
            ),
            E_Fun "and" ["a", "b"]
            (
              E_If
                (E_And (E_Var "a") (E_Var "b"))
                (E_Value 1)
                (E_Value 0)
            ),
            E_Fun "suma" ["a", "b"]
            (
              E_BinExpr E_Add (E_Var "a") (E_Var "b")
            ),
            E_Fun "multiplicar" ["a", "b"]
            (
              E_BinExpr E_Mul (E_Var "a") (E_Var "b")
            ),
            E_Fun "color_a_vel" ["gris"]
            (
              E_If
                (E_Compare E_GT (E_Var "gris") (E_Var "MIN_GREY"))
                (E_Value 10)
                (E_Value 5)
            )]
            (E_Do [
              E_Read "distance" "INPUT_DISTANCE",
              E_Read "color_izq" "INPUT_COLOR_LEFT",
              E_Read "color_der" "INPUT_COLOR_RIGHT",

              E_Lift "viendo_casa" "hay_casa" "distance",
              E_Folds "cambio" "distinto" (E_Value 0) "viendo_casa",
              E_Lift2 "nueva_casa" "and" "viendo_casa" "cambio",
              E_Folds "cuenta" "suma" (E_Value 0) "nueva_casa",
              E_Lift "velocidad" "velocidad_casa" "cuenta",
              
              E_Lift "multip_izq" "color_a_vel" "color_izq",
              E_Lift "multip_der" "color_a_vel" "color_der",

              E_Lift2 "speed_left" "multiplicar" "velocidad" "multip_izq",
              E_Lift2 "speed_right" "multiplicar" "velocidad" "multip_der",

              E_Output "OUTPUT_ENGINE_LEFT" "speed_left",
              E_Output "OUTPUT_ENGINE_RIGHT" "speed_right"
            ])

main = do
  putStrLn "Alf? Willie? Alf? Willie? Alf! Willie!"
  putStrLn $ show program
  putStrLn "Compilando... Alf? Willie!"
  putStrLn $ show (generate_bytecode program)
