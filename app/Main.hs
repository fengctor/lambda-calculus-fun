module Main where

import LambdaCalculus.Infer
import LambdaCalculus.Types

exampleExpr :: LCExpr
exampleExpr =
  Abs "f" $ ArithBinOp Add (ArithBinOp Mul (App (Var "f") (Int 1)) (Int 2)) (App (Abs "x" (Var "x")) (Int 3))

main :: IO ()
main = do
  print exampleExpr
  print $ inferType exampleExpr
