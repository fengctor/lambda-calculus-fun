module LambdaCalculus.Types.LCExpr where

data LCExpr
  = Var String
  | Abs String LCExpr
  | App LCExpr LCExpr
  | Int Int
  | ArithBinOp ArithOp LCExpr LCExpr

data ArithOp
  = Add
  | Sub
  | Mul
  | Div

instance Show ArithOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

showsPrecInfixl :: (Show s) => Int -> Int -> s -> String -> s -> ShowS
showsPrecInfixl d prec s1 opStr s2 =
  showParen (d > prec) $
    showsPrec prec s1 . showString opStr . showsPrec (prec + 1) s2

instance Show LCExpr where
  showsPrec _ (Var v) = showString v
  showsPrec d (Abs v e) = showParen (d > 6) $ showString ("\\" ++ v ++ " -> ") . showsPrec 6 e
  showsPrec d (App e1 e2) = showsPrecInfixl d 8 e1 " " e2
  showsPrec d (ArithBinOp Add e1 e2) = showsPrecInfixl d 6 e1 " + " e2
  showsPrec d (ArithBinOp Sub e1 e2) = showsPrecInfixl d 6 e1 " - " e2
  showsPrec d (ArithBinOp Mul e1 e2) = showsPrecInfixl d 7 e1 " * " e2
  showsPrec d (ArithBinOp Div e1 e2) = showsPrecInfixl d 7 e1 " / " e2
  showsPrec _ (Int n) = showString (show n)
