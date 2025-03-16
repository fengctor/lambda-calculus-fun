module LambdaCalculus.Infer (
  inferType,
) where

import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.State (MonadState (..), State, evalState, modify')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import LambdaCalculus.Types.LCExpr (LCExpr (..))
import LambdaCalculus.Types.LCType (LCType (..))

newtype Substitution = Substitution {mapping :: IntMap LCType}

runSubstitution :: Substitution -> LCType -> LCType
runSubstitution subst (TypeVar i) = IntMap.findWithDefault (TypeVar i) i (mapping subst)
runSubstitution subst (FunctionType t1 t2) =
  FunctionType (runSubstitution subst t1) (runSubstitution subst t2)
runSubstitution _ IntType = IntType

instance Semigroup Substitution where
  -- Composition of Substitutions, such that:
  -- ```
  -- runSubstitution (subst2 <> subst1) t1 = runSubstitution subst2 (runSubstitution subst1 t1).
  -- ```
  -- We want to result in a mapping where for each source index `i`:
  -- - if `i` is in the mapping of `subst1`, rewrite its associated value to the result of running `subst2` on it.
  -- - if `i` is only in the mapping of `subst2`, keep its entry.
  -- The latter is needed since `i` being missing from `subst1` is equivalent to it mapping to `TypeVar i`.
  subst2 <> subst1 =
    Substitution $
      IntMap.union (mapping subst2) $
        fmap (runSubstitution subst2) (mapping subst1)

instance Monoid Substitution where
  mempty = Substitution IntMap.empty

singletonSubstitution :: Int -> LCType -> Substitution
singletonSubstitution i t = Substitution $ IntMap.singleton i t

data InferError
  = InfiniteType
  | UnificationFailure
  | UnboundVariable String
  deriving (Show)

getFreshVarTypeIndex :: ExceptT InferError (State Int) Int
getFreshVarTypeIndex = do
  i <- get
  modify' (+ 1)
  pure i

inferType :: LCExpr -> Either InferError LCType
inferType expr = fst <$> evalState (runExceptT (go Map.empty expr)) 0
 where
  go :: Map String LCType -> LCExpr -> ExceptT InferError (State Int) (LCType, Substitution)
  go typeCtx (Var v) = liftEither $ case Map.lookup v typeCtx of
    Nothing -> Left $ UnboundVariable v
    Just t -> Right (t, mempty)
  go typeCtx (Abs v e) = do
    freshVarIndex <- getFreshVarTypeIndex
    let freshTypeVar = TypeVar freshVarIndex
    let nextTypeCtx = Map.insert v freshTypeVar typeCtx
    (eType, eSubst) <- go nextTypeCtx e
    pure (FunctionType (runSubstitution eSubst freshTypeVar) eType, eSubst)
  go typeCtx (App e1 e2) = do
    (e1Type, e1Subst) <- go typeCtx e1
    let e1UpdatedTypeCtx = fmap (runSubstitution e1Subst) typeCtx
    (e2Type, e2Subst) <- go e1UpdatedTypeCtx e2
    freshVarIndex <- getFreshVarTypeIndex
    let resultTypeVar = TypeVar freshVarIndex
    let unifyTarget = FunctionType e2Type resultTypeVar
    unifySubst <- liftEither $ tryUnify (runSubstitution e2Subst e1Type) unifyTarget
    let resultTy = runSubstitution unifySubst resultTypeVar
    pure
      ( resultTy
      , unifySubst <> e2Subst <> e1Subst
      )
  go _ (Int _) = pure (IntType, mempty)
  go typeCtx (ArithBinOp _ e1 e2) = do
    (e1Type, e1InitialSubst) <- go typeCtx e1
    e1UnifyIntSubst <- liftEither $ tryUnify e1Type IntType
    let e1Subst = e1UnifyIntSubst <> e1InitialSubst
    let e1UpdatedTypeCtx = fmap (runSubstitution e1Subst) typeCtx
    (e2Type, e2InitialSubst) <- go e1UpdatedTypeCtx e2
    e2UnifyIntSubst <- liftEither $ tryUnify e2Type IntType
    let e2Subst = e2UnifyIntSubst <> e2InitialSubst
    pure (IntType, e2Subst <> e1Subst)

occursCheck :: Int -> LCType -> Either InferError ()
occursCheck i (TypeVar i')
  | i == i' = Left InfiniteType
  | otherwise = Right ()
occursCheck i (FunctionType t1 t2) = occursCheck i t1 *> occursCheck i t2
occursCheck _ IntType = Right ()

tryUnify :: LCType -> LCType -> Either InferError Substitution
tryUnify (TypeVar i) t = singletonSubstitution i t <$ occursCheck i t
tryUnify t (TypeVar i) = singletonSubstitution i t <$ occursCheck i t
tryUnify IntType IntType = Right mempty
tryUnify (FunctionType ty1 ty2) (FunctionType ty3 ty4) = do
  subst1 <- tryUnify ty1 ty3
  let substitutedTy2 = runSubstitution subst1 ty2
  let substitutedTy4 = runSubstitution subst1 ty4
  subst2 <- tryUnify substitutedTy2 substitutedTy4
  let resultSubstitution = subst2 <> subst1
  pure resultSubstitution
tryUnify _ _ = Left UnificationFailure
