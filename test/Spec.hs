module Main (main) where

import Prelude
import Test.Tasty
import Test.Tasty.Hspec

import qualified TestParser

main :: IO ()
main = TestParser.main
  -- parserTests <- TestParser.main
  -- defaultMain $ testGroup "Tests" [ parserTests ]
