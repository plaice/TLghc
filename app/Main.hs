module Main where

import System.IO
import System.Environment
import TLparse

main :: IO ()
main = getArgs >>= parse >>= playFile

parse []     = getContents
parse fs     = fmap concat $ mapM readFile fs
