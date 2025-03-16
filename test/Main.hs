module Main (main) where

import Test.Hspec

import Substitution

main :: IO ()
main = hspec $ describe "Substitution" substitutionSpec
