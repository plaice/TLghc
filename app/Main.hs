{- |
Module      : Main
Description : The Main module for TLghc
Copyright   : (c) John Plaice, 2020
License     : GPL-3
Maintainer  : johnplaice@gmail.com
Stability   : experimental
Portability : Portable
-}
module Main where

import Control.Applicative
import System.IO
import System.Environment
import TL

main :: IO ()
main = do
  args <- getArgs
  let (printAST, parseOnly, files) = readArgs args
  programText <- gather files
  let result = processText programText printAST parseOnly
  case result of
    Left err -> error err
    Right output -> putStr output

readArgs :: [String] -> (Bool, Bool, [String])
readArgs [] = (False, False, [])
readArgs ("--parseOnly" : "--printAST" : args) = (True, True, args)
readArgs ("--printAST" : "--parseOnly" : args) = (True, True, args)
readArgs ("--printAST" : args) = (True, False, args)
readArgs ("--parseOnly" : args) = (False, True, args)
readArgs args = (False, False, args)

gather :: [FilePath] -> IO String
gather [] = getContents
gather fs = concat <$> mapM readFile fs
