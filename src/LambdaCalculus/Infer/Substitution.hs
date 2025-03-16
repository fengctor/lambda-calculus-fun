module LambdaCalculus.Infer.Substitution where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import LambdaCalculus.Types.LCType (LCType (..))

newtype Substitution
  = Substitution {mapping :: IntMap LCType}
  deriving (Show)

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
