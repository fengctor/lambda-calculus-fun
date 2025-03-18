module Main where

import System.IO

import LambdaCalculus.Infer
import LambdaCalculus.Parser
import LambdaCalculus.Reduce
import LambdaCalculus.Types

exampleExpr :: LCExpr String
exampleExpr =
  Abs "f" $ ArithBinOp Add (ArithBinOp Mul (App (Var "f") (Int 1)) (Int 2)) (App (Abs "x" (Var "x")) (Int 3))

main :: IO ()
main = hSetBuffering stdout NoBuffering *> repl

repl :: IO ()
repl = do
  putStr "lci> "
  input <- getLine
  if input == ""
    then pure ()
    else case parseLCExpr input of
      Nothing -> putStrLn $ "Failed to parse: " ++ input
      Just expr -> case inferType expr of
        Left err -> putStrLn $ "Error inferring type of " ++ input ++ ": " ++ show err
        Right ty -> do
          putStrLn $ input ++ " :: " ++ show ty
          case relabelToUniqueBinders expr of
            Nothing -> putStrLn "Error relabeling binders (this shouldn't happen after type inference)"
            Just relabeledExpr -> do
              print $ betaReduceUniqueBinders relabeledExpr
  repl
