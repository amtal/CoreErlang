module TestParser where

import           Control.Monad

import           Language.CoreErlang
import           System.Directory
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO TestTree
main = testSpec "parser" spec

spec :: Spec
spec = do
  fps <-runIO $ getCoreFiles "."
  forM_ fps testFun

moduleName (Constr (Module name _ _ _)) = name
moduleName (Ann (Module name _ _ _) _)  = name


testFun :: FilePath -> Spec
testFun f = do
  context "testParser" $ do
    specify f $ do
      r <- parseFile f
      case r of
        Left r       -> expectationFailure (show r)
        Right annMod -> return ()
        --moduleName annMod `shouldBe` (Atom "jsx")


isCoreFile :: String -> Bool
isCoreFile fname = (== "eroc") $ take 4 $ reverse $ fname


getCoreFiles :: FilePath -> IO [FilePath]
getCoreFiles basePath = do
  list <- listDirectory basePath
  r <- forM list $ \filePath -> do
    let tp = basePath ++ "/" ++ filePath
    res <- doesDirectoryExist tp
    if res
      then getCoreFiles tp
      else if isCoreFile filePath
              then return [tp]
                   else return []
  return $ concat r





