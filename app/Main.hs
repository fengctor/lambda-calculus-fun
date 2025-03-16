module Main where

import LambdaCalculus.Infer
import LambdaCalculus.Reduce
import LambdaCalculus.Types

exampleExpr :: LCExpr String
exampleExpr =
  Abs "f" $ ArithBinOp Add (ArithBinOp Mul (App (Var "f") (Int 1)) (Int 2)) (App (Abs "x" (Var "x")) (Int 3))

main :: IO ()
main = do
  print exampleExpr
  print $ inferType exampleExpr
  print $ relabelToUniqueBinders exampleExpr
  let applied = App exampleExpr (Abs "x" (ArithBinOp Mul (Var "x") (Int 10)))
  print applied
  case relabelToUniqueBinders applied of
    Nothing -> putStrLn "relabeling failed"
    Just relabeledApplied -> print $ betaReduceUniqueBinders relabeledApplied
