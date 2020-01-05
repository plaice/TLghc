module Main where

import System.IO
import TL
import TLparse

main :: IO ()
main = do
  str <- readFile "simpleExamples"
  print str
  playFile str

