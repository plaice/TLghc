module Main where

import System.IO
import System.Environment
import TLparse

main :: IO ()
main = do
  args <- getArgs
  (printAST
  ast <- parse files
  playFile ast

parse []     = getContents
parse fs     = fmap concat $ mapM readFile fs
