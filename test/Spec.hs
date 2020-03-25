module Main (main) where

import Prelude
import Test.Tasty
import Test.Tasty.Hspec

import qualified TestParser

main :: IO ()
main = do
  parserTests <- TestParser.main
  defaultMain $ testGroup "Tests" [ parserTests ]

