module TestParser where

import Test.Tasty
import Test.Tasty.Hspec
import Language.CoreErlang

main :: IO TestTree
main = testSpec "parser" spec

spec :: Spec
spec = do
  context "SimpleParser" $ do
      specify "should parse the 'Hello' module" $ do
        r <- parseFile "test/data/Hello.core"
        case r of
          Left _ -> return ()
          Right annMod -> moduleName annMod `shouldBe` (Atom "Hello")
      specify "should parse the 'Map' module" $ do
        r <- parseFile "test/data/Map.core"
        case r of
          Left _ -> return ()
          Right annMod -> moduleName annMod `shouldBe` (Atom "Map")
  context "JsxParser" $ do
    specify "should parse the 'jsx' module" $ do
      r <- parseFile "test/data/jsx/jsx.core"
      case r of
        Left _ -> return ()
        Right annMod -> moduleName annMod `shouldBe` (Atom "jsx")
  context "SaslParser" $ do
    specify "should parse the 'sasl' module" $ do
      r <- parseFile "test/data/sasl/sasl.core"
      case r of
        Left _ -> return ()
        Right annMod -> moduleName annMod `shouldBe` (Atom "jsx")
  context "StdlibParser" $ do
    specify "should parse the 'stdlib' module" $ do
      r <- parseFile "test/data/stdlib/array.core"
      case r of
        Left _ -> return ()
        Right annMod -> moduleName annMod `shouldBe` (Atom "jsx")

moduleName (Constr (Module name _ _ _)) = name
moduleName (Ann (Module name _ _ _) _) = name

