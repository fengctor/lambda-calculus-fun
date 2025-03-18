module Main (main) where

import Test.Hspec

import Parser
import Substitution

main :: IO ()
main = hspec $ do
  describe "Substitution" substitutionSpec
  describe "Parser" parserSpec
