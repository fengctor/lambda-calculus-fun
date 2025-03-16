module Main where

import LambdaCalculus.Types

main :: IO ()
main = do
  print $ Abs "f" $ ArithBinOp Add (ArithBinOp Mul (App (Var "f") (Int 1)) (Int 2)) (App (Abs "x" (Var "x")) (Int 3))
  print $ FunctionType (FunctionType IntType IntType) IntType
