module Parser where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import LambdaCalculus.Parser
import LambdaCalculus.Types.LCExpr

parsedLCExprGen :: Int -> Gen (LCExpr String)
parsedLCExprGen maxH
  | maxH <= 0 =
      oneof
        [ Var <$> identGen
        , Int . getNonNegative <$> arbitrary @(NonNegative Int)
        ]
  | otherwise =
      oneof
        [ Var <$> identGen
        , Int . getNonNegative <$> arbitrary @(NonNegative Int)
        , Abs <$> identGen <*> parsedLCExprGen (maxH - 1)
        , App <$> parsedLCExprGen (maxH - 1) <*> parsedLCExprGen (maxH - 1)
        , ArithBinOp Add <$> parsedLCExprGen (maxH - 1) <*> parsedLCExprGen (maxH - 1)
        , ArithBinOp Sub <$> parsedLCExprGen (maxH - 1) <*> parsedLCExprGen (maxH - 1)
        , ArithBinOp Mul <$> parsedLCExprGen (maxH - 1) <*> parsedLCExprGen (maxH - 1)
        , ArithBinOp Div <$> parsedLCExprGen (maxH - 1) <*> parsedLCExprGen (maxH - 1)
        ]

identGen :: Gen String
identGen = (:) <$> alphaGen <*> listOf (oneof [alphaGen, digitGen])
 where
  alphaGen :: Gen Char
  alphaGen = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

  digitGen :: Gen Char
  digitGen = elements ['0' .. '9']

parsePretty :: Property
parsePretty = forAll (chooseInt (0, 31)) $ \maxHeight ->
  forAll (parsedLCExprGen maxHeight) $ \expr ->
    parseLCExpr (show (Pretty expr)) == Just expr

parserPropertiesSpec :: Spec
parserPropertiesSpec = do
  prop "can parse pretty-printed LCExprs" $ within 1000000 parsePretty

parserSpec :: Spec
parserSpec = parserPropertiesSpec
