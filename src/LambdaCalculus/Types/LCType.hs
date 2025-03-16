module LambdaCalculus.Types.LCType where

data LCType
  = TypeVar Int
  | FunctionType LCType LCType
  | IntType
  deriving (Eq)

instance Show LCType where
  showsPrec _ (TypeVar i) = showString $ "t" ++ show i
  showsPrec d (FunctionType t1 t2) =
    showParen (d > 5) $ showsPrec 6 t1 . showString " -> " . showsPrec 5 t2
  showsPrec _ IntType = showString "Int"
