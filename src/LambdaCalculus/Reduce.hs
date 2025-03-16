module LambdaCalculus.Reduce where

import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import LambdaCalculus.Types.LCExpr

relabelToUniqueBinders :: forall l. (Ord l) => LCExpr l -> Maybe (LCExpr Int)
relabelToUniqueBinders expr = evalState (go Map.empty expr) 0
 where
  go :: Map l Int -> LCExpr l -> State Int (Maybe (LCExpr Int))
  go relabelCtx (Var l) = pure $ Var <$> Map.lookup l relabelCtx
  go relabelCtx (Abs l e) = do
    newLabel <- get
    modify' (+ 1)
    let nextRelabelCtx = Map.insert l newLabel relabelCtx
    mRelabeledE <- go nextRelabelCtx e
    pure $ Abs newLabel <$> mRelabeledE
  go relabelCtx (App e1 e2) = do
    mRelabeledE1 <- go relabelCtx e1
    mRelabeledE2 <- go relabelCtx e2
    pure $ App <$> mRelabeledE1 <*> mRelabeledE2
  go _ (Int n) = pure $ Just $ Int n
  go relabelCtx (ArithBinOp op e1 e2) = do
    mRelabeledE1 <- go relabelCtx e1
    mRelabeledE2 <- go relabelCtx e2
    pure $ ArithBinOp op <$> mRelabeledE1 <*> mRelabeledE2

-- Capture-avoiding substitution, assuming the binders across the given 'LCExpr's are unique.
substituteUniqueBinders :: forall l. (Eq l) => l -> LCExpr l -> LCExpr l -> LCExpr l
substituteUniqueBinders label forExpr = go
 where
  go :: LCExpr l -> LCExpr l
  go (Var l) = if l == label then forExpr else Var l
  go (Abs l e) = Abs l (go e)
  go (App e1 e2) = App (go e1) (go e2)
  go (Int n) = Int n
  go (ArithBinOp op e1 e2) = ArithBinOp op (go e1) (go e2)

applyArithOp :: ArithOp -> Int -> Int -> Int
applyArithOp Add n1 n2 = n1 + n2
applyArithOp Sub n1 n2 = n1 - n2
applyArithOp Mul n1 n2 = n1 * n2
applyArithOp Div n1 n2 = n1 `div` n2

-- | Beta reduction of a 'LCExpr' with unique 'Int' binders.
betaReduceUniqueBinders :: LCExpr Int -> LCExpr Int
betaReduceUniqueBinders (Var v) = Var v
betaReduceUniqueBinders (Abs l e) = Abs l e
betaReduceUniqueBinders (App e1 e2) = case (betaReduceUniqueBinders e1, betaReduceUniqueBinders e2) of
  (Abs l e, reducedE2) -> betaReduceUniqueBinders $ substituteUniqueBinders l reducedE2 e
  (reducedE1, reducedE2) -> App reducedE1 reducedE2
betaReduceUniqueBinders (Int n) = Int n
betaReduceUniqueBinders (ArithBinOp op e1 e2) = case (betaReduceUniqueBinders e1, betaReduceUniqueBinders e2) of
  (Int n1, Int n2) -> Int $ applyArithOp op n1 n2
  (reducedE1, reducedE2) -> ArithBinOp op reducedE1 reducedE2
