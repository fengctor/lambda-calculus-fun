module LambdaCalculus.Types.LCExpr where

data LCExpr l
  = Var l
  | Abs l (LCExpr l)
  | App (LCExpr l) (LCExpr l)
  | Int Int
  | ArithBinOp ArithOp (LCExpr l) (LCExpr l)
  deriving (Show, Eq)

data ArithOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

newtype Pretty a = Pretty a

showsPrecInfixl :: (Show s) => Int -> Int -> s -> String -> s -> ShowS
showsPrecInfixl d prec s1 opStr s2 =
  showParen (d > prec) $
    showsPrec prec s1 . showString opStr . showsPrec (prec + 1) s2

instance Show (Pretty (LCExpr String)) where
  showsPrec _ (Pretty (Var v)) = showString v
  showsPrec d (Pretty (Abs v e)) = showParen (d > 5) $ showString ("\\" ++ v ++ " -> ") . showsPrec 5 (Pretty e)
  showsPrec d (Pretty (App e1 e2)) = showsPrecInfixl d 8 (Pretty e1) " " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Add e1 e2)) = showsPrecInfixl d 6 (Pretty e1) " + " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Sub e1 e2)) = showsPrecInfixl d 6 (Pretty e1) " - " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Mul e1 e2)) = showsPrecInfixl d 7 (Pretty e1) " * " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Div e1 e2)) = showsPrecInfixl d 7 (Pretty e1) " / " (Pretty e2)
  showsPrec _ (Pretty (Int n)) = showString (show n)

instance Show (Pretty (LCExpr Int)) where
  showsPrec _ (Pretty (Var v)) = showString $ "x_" ++ show v
  showsPrec d (Pretty (Abs v e)) = showParen (d > 5) $ showString ("\\x_" ++ show v ++ " -> ") . showsPrec 5 (Pretty e)
  showsPrec d (Pretty (App e1 e2)) = showsPrecInfixl d 8 (Pretty e1) " " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Add e1 e2)) = showsPrecInfixl d 6 (Pretty e1) " + " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Sub e1 e2)) = showsPrecInfixl d 6 (Pretty e1) " - " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Mul e1 e2)) = showsPrecInfixl d 7 (Pretty e1) " * " (Pretty e2)
  showsPrec d (Pretty (ArithBinOp Div e1 e2)) = showsPrecInfixl d 7 (Pretty e1) " / " (Pretty e2)
  showsPrec _ (Pretty (Int n)) = showString (show n)
