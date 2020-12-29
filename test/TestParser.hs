module TestParser where

import Control.Monad
import Language.CoreErlang
import System.Directory
import System.FilePath.Posix(takeDirectory)
import Test.Tasty
import Test.Tasty.Hspec
import Text.Megaparsec (errorBundlePretty)
import System.IO (openFile, hClose, IOMode(..))
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc

main :: IO ()
main = do
  fps <- getCoreFiles "./test/data/jsx"
  mapM generatePrettyCore fps
  -- spec <- testSpec "ParseThenPretty" specCompare
  defaultMain $ testGroup "Tests" []


spec :: Spec
spec = do
  fps <- runIO $ getCoreFiles "./test"
  forM_ fps testParser
--
specPretty :: Spec
specPretty = do
  fps <- runIO $ getCoreFiles "./test/generated"
  forM_ fps testPretty

specCompare :: Spec
specCompare = do
  fps <- runIO $ getCoreFiles "./test/data"
  fps' <- runIO $ getCoreFiles "./test/generated"
  forM_ (zip fps fps') testParseThenPretty

testParseThenPretty :: (FilePath, FilePath) -> Spec
testParseThenPretty (f, f') = do
  specify (f <> f') $ do
    r <- parseFile f
    r' <- parseFile f'
    if r == r' then return () else expectationFailure "testFail"

moduleName (Module name _ _ _ _)  = name

testParser :: FilePath -> Spec
testParser f = do
  specify f $ do
    r <- parseFile f
    case r of
      Left r       -> expectationFailure (errorBundlePretty r)
      Right annMod -> return ()

testPretty :: FilePath -> Spec
testPretty f = do
  specify f $ do
    r <- parseFile f
    case r of
      Left r       -> expectationFailure (errorBundlePretty r)
      Right annMod -> return ()

generatePrettyCore :: FilePath -> IO ()
generatePrettyCore fp =
  do x <- parseFile fp
     let new = "./test/generated/" ++ drop 6 fp
     createDirectoryIfMissing True $ (takeDirectory new)
     h <- openFile new WriteMode
     renderIO h (layoutSmart defaultLayoutOptions (pretty x))
     hClose h


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
