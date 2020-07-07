-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErlang
-- Copyright   :  (c) Feng Lee 2020
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- CoreErlang syntax, parser and pretty printer.
--
-----------------------------------------------------------------------------
module Language.CoreErlang
  ( module CE
  -- , parseAndPretty
  , parseFile
  ) where

import Language.CoreErlang.Parser as CE
-- import Language.CoreErlang.Pretty as CE
import Language.CoreErlang.Syntax as CE
import Data.Text.IO (readFile)
import Prelude hiding (readFile)
-- import Data.Text.Prettyprint.Doc.Render.Text
-- import System.IO (openFile, hClose, IOMode(..))
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath.Posix(takeDirectory)
--
-- parseAndPretty :: FilePath -> IO ()
-- parseAndPretty fp =
--   do x <- parseFile fp
--      let new = "generated/" ++ fp
--      createDirectoryIfMissing True $ (takeDirectory new)
--      h <- openFile new WriteM
--      hPutDoc h (pretty x)
--      hClose h

--
-- printResult :: (Either ParseErr (Ann Module)) -> IO ()
-- printResult (Left err) = print err
-- printResult (Right annMod) = putStrLn $ prettyPrint annMod

parseFile :: FilePath -> IO (Either ParseErr (Module Text))
parseFile fp = readFile fp >>= return . parseModuleA


-- readCoreFile :: FilePath -> IO Text
-- readCoreFile fp = do
--   h <- openFile fp ReadMode
--   hSetEncoding h utf8
--   hGetContents h
