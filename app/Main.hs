module Main where

import LambdaCalculus.Types

main :: IO ()
main = do
  print $ Abs "x" $ ArithBinOp Add (ArithBinOp Mul (Var "x") (Int 1)) (App (Abs "x" (Var "x")) (Int 2))
