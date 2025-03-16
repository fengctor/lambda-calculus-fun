module LambdaCalculus.Types.LCExpr where

data LCExpr l
  = Var l
  | Abs l (LCExpr l)
  | App (LCExpr l) (LCExpr l)
  | Int Int
  | ArithBinOp ArithOp (LCExpr l) (LCExpr l)

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

instance Show (LCExpr String) where
  showsPrec _ (Var v) = showString v
  showsPrec d (Abs v e) = showParen (d > 6) $ showString ("\\" ++ v ++ " -> ") . showsPrec 6 e
  showsPrec d (App e1 e2) = showsPrecInfixl d 8 e1 " " e2
  showsPrec d (ArithBinOp Add e1 e2) = showsPrecInfixl d 6 e1 " + " e2
  showsPrec d (ArithBinOp Sub e1 e2) = showsPrecInfixl d 6 e1 " - " e2
  showsPrec d (ArithBinOp Mul e1 e2) = showsPrecInfixl d 7 e1 " * " e2
  showsPrec d (ArithBinOp Div e1 e2) = showsPrecInfixl d 7 e1 " / " e2
  showsPrec _ (Int n) = showString (show n)

instance Show (LCExpr Int) where
  showsPrec _ (Var v) = showString $ "x_" ++ show v
  showsPrec d (Abs v e) = showParen (d > 6) $ showString ("\\x_" ++ show v ++ " -> ") . showsPrec 6 e
  showsPrec d (App e1 e2) = showsPrecInfixl d 8 e1 " " e2
  showsPrec d (ArithBinOp Add e1 e2) = showsPrecInfixl d 6 e1 " + " e2
  showsPrec d (ArithBinOp Sub e1 e2) = showsPrecInfixl d 6 e1 " - " e2
  showsPrec d (ArithBinOp Mul e1 e2) = showsPrecInfixl d 7 e1 " * " e2
  showsPrec d (ArithBinOp Div e1 e2) = showsPrecInfixl d 7 e1 " / " e2
  showsPrec _ (Int n) = showString (show n)
