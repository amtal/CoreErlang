-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErlang.Pretty
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
  , parseAndPretty
  , parseFile
  ) where

import Language.CoreErlang.Parser as CE
import Language.CoreErlang.Pretty as CE
import Language.CoreErlang.Syntax as CE

import System.IO

parseAndPretty :: FilePath -> IO ()
parseAndPretty fp = parseFile fp >>= printResult

printResult :: (Either ParseError (Ann Module)) -> IO ()
printResult (Left err) = print err
printResult (Right annMod) = putStrLn $ prettyPrint annMod

parseFile :: FilePath -> IO (Either ParseError (Ann Module))
parseFile fp = readCoreFile fp >>= return . parseModule

readCoreFile :: FilePath -> IO String
readCoreFile fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  hGetContents h

