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

readArgs [] = (False, False, [])
readArgs ("--parseOnly" : "--printAST" : args) = (True, True, args)
readArgs ("--printAST" : "--parseOnly" : args) = (True, True, args)
readArgs ("--printAST" : args) = (True, False, args)
readArgs ("--parseOnly" : args) = (False, True, args)
readArgs args = (False, False, args)

gather [] = getContents
gather fs = concat Control.Applicative.<$> mapM readFile fs
