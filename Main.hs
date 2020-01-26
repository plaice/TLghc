module Main where

import System.IO
import System.Environment
import System.Exit
import TL
import TLparse

main :: IO ()
main = getArgs >>= parse >>= playFile

parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs
