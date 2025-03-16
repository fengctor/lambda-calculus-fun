module Substitution where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.IntMap.Strict qualified as IntMap

import LambdaCalculus.Infer.Substitution (Substitution (..), runSubstitution)
import LambdaCalculus.Types.LCType (LCType (..))

{- | 'indexBoundSubstGen maxIndex' is a generator for 'Substitution's involving type variables with
indices in [1..size]
-}
indexBoundSubstGen :: Int -> Gen Substitution
indexBoundSubstGen maxIndex = do
  let maxIndexBoundTyGen = indexBoundTyGen maxIndex
  substVarIndices <- sublistOf [1 .. maxIndex]
  entries <- traverse (\i -> (i,) <$> maxIndexBoundTyGen) substVarIndices
  pure $ Substitution $ IntMap.fromList entries

{- | 'indexBoundSubstGen maxIndex' is a generator for 'Substitution's involving type
 - variables with indices i in [1..size] mapping to themselves (TypeVar i)
-}
indexBoundInertSubstGen :: Int -> Gen Substitution
indexBoundInertSubstGen maxIndex =
  Substitution
    . IntMap.fromList
    . fmap (\i -> (i, TypeVar i))
    <$> sublistOf [1 .. maxIndex]

{- | 'indexBoundTyGen maxIndex' is a generator for 'LCType's involving type variables with indices
in [1..size]
-}
indexBoundTyGen :: Int -> Gen LCType
indexBoundTyGen maxIndex = go
 where
  go =
    oneof
      [ TypeVar <$> chooseInt (1, maxIndex)
      , FunctionType <$> go <*> go
      , pure IntType
      ]

memptyLeftIdentity :: Property
memptyLeftIdentity = forAll arbitrary $ \(NonNegative i) ->
  forAll (indexBoundSubstGen i) $ \subst ->
    forAll (indexBoundTyGen i) $ \ty ->
      runSubstitution (mempty <> subst) ty == runSubstitution subst ty

memptyRightIdentity :: Property
memptyRightIdentity = forAll arbitrary $ \(NonNegative i) ->
  forAll (indexBoundSubstGen i) $ \subst ->
    forAll (indexBoundTyGen i) $ \ty ->
      runSubstitution (subst <> mempty) ty == runSubstitution subst ty

mappendAssociative :: Property
mappendAssociative = forAll arbitrary $ \(NonNegative i) ->
  let
    iBoundSubstGen = indexBoundSubstGen i
   in
    forAll ((,,) <$> iBoundSubstGen <*> iBoundSubstGen <*> iBoundSubstGen) $ \(subst1, subst2, subst3) ->
      forAll (indexBoundTyGen i) $ \ty ->
        runSubstitution (subst1 <> (subst2 <> subst3)) ty == runSubstitution ((subst1 <> subst2) <> subst3) ty

substitutionMonoidLawsSpec :: Spec
substitutionMonoidLawsSpec = do
  prop "left identity" memptyLeftIdentity
  prop "right identity" memptyRightIdentity
  prop "associative" mappendAssociative

runComposition :: Property
runComposition = forAll arbitrary $ \(NonNegative i) ->
  let
    iBoundSubstGen = indexBoundSubstGen i
   in
    forAll ((,) <$> iBoundSubstGen <*> iBoundSubstGen) $ \(subst1, subst2) ->
      forAll (indexBoundTyGen i) $
        \ty ->
          runSubstitution (subst2 <> subst1) ty == runSubstitution subst2 (runSubstitution subst1 ty)

substitutionCompositionSpec :: Spec
substitutionCompositionSpec = do
  prop
    "running a composed Substitution is equivalent to running the Substitutions one after another"
    runComposition

inertSubstitution :: Property
inertSubstitution = forAll arbitrary $ \(NonNegative i) ->
  forAll (indexBoundInertSubstGen i) $ \subst ->
    forAll (indexBoundTyGen i) $ \ty ->
      runSubstitution subst ty == runSubstitution mempty ty

inertSubstitutionSpec :: Spec
inertSubstitutionSpec = do
  prop
    "a Substitution that only maps indices to their own TypeVars is inert"
    inertSubstitution

substitutionPropertiesSpec :: Spec
substitutionPropertiesSpec = do
  describe "Monoid laws" substitutionMonoidLawsSpec
  describe "Composition" substitutionCompositionSpec
  describe "Inert" inertSubstitutionSpec

substitutionSpec :: Spec
substitutionSpec = substitutionPropertiesSpec
