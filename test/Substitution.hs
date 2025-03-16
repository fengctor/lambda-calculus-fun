module Substitution where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.IntMap.Strict qualified as IntMap

import LambdaCalculus.Infer.Substitution (Substitution (..), runSubstitution)
import LambdaCalculus.Types.LCType (LCType (..))

{- | 'sizeBoundSubstGen size' is a generator for 'Substitution's involving type variables with
indices in [1..size]
-}
sizeBoundSubstGen :: Int -> Gen Substitution
sizeBoundSubstGen size =
  Substitution . IntMap.fromList
    <$> fmap
      (zip [1 .. size])
      ( sequenceA $
          replicate size $
            indexBoundTyGen size
      )

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
  forAll (sizeBoundSubstGen i) $ \subst ->
    forAll (indexBoundTyGen i) $ \ty ->
      runSubstitution (mempty <> subst) ty == runSubstitution subst ty

memptyRightIdentity :: Property
memptyRightIdentity = forAll arbitrary $ \(NonNegative i) ->
  forAll (sizeBoundSubstGen i) $ \subst ->
    forAll (indexBoundTyGen i) $ \ty ->
      runSubstitution (subst <> mempty) ty == runSubstitution subst ty

mappendAssociative :: Property
mappendAssociative = forAll arbitrary $ \(NonNegative i) ->
  let
    iBoundSubstGen = sizeBoundSubstGen i
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
    iBoundSubstGen = sizeBoundSubstGen i
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

substitutionPropertiesSpec :: Spec
substitutionPropertiesSpec = do
  describe "Monoid laws" substitutionMonoidLawsSpec
  describe "Composition" substitutionCompositionSpec

substitutionSpec :: Spec
substitutionSpec = substitutionPropertiesSpec
