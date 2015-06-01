

data Alfcode = Tpush Int
             | Tstore Int
             | Tread Int Int
             | Tlift Int Int String
             deriving (Eq, Ord, Show, Read)

instruction_length :: Alfcode -> Int
instruction_length (Tpush _) = 2
instruction_length (Tstore _) = 1
instruction_length (Tread _ _) = 2
instruction_length (Tlift _ _ _) = 3

data BinOp = E_Add | E_Sub | E_Mul | E_Div
           deriving Show

data BinComp = E_GT | E_LT | E_GTe | E_LTe | E_Eq | E_Neq
             deriving Show

data BoolExpr = E_Compare BinComp Expression Expression
              | E_And Expression Expression 
              | E_Or Expression Expression
              | E_Not Expression
              deriving Show

data Expression = E_Value Int
                | E_BinExpr BinOp Expression Expression
                | E_Var String
                | E_BoolExpr BoolExpr
                | E_If BoolExpr Expression Expression
                deriving Show

data Declaration = E_Fun [Char] [String] Expression
                 | E_Const String Int
                 deriving Show

data FRPApplication = E_Read String String
                    | E_Lift String String String
                    | E_Lift2 String String String String
                    | E_Folds String String Expression String
                    | E_Output String String
                    deriving Show

data DoDeclaration = E_Do [FRPApplication]
                   deriving Show

data Program = E_Program [Declaration] DoDeclaration
             deriving Show

program :: Program
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
            E_Fun "color_a_vel" ["gris"]
            (
              E_If
                (E_Compare E_GT (E_Var "gris") (E_Var "MIN_GREY"))
                (E_Value 10)
                (E_Value 5)
            ),
            E_Fun "multiplicar" ["a", "b"]
            (
              E_BinExpr E_Mul (E_Var "a") (E_Var "b")
            )
            ]
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
