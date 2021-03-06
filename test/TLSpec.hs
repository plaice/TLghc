{- |
Module      : Main
Description : Top-level file for testing TLghc
Copyright   : (c) John Plaice, 2020
License     : GPL-3
Maintainer  : johnplaice@gmail.com
Stability   : experimental
Portability : Portable
-}
module Main where

import Test.Hspec
import System.IO
import TL

main :: IO ()
main = do
  testOne "empty"
  testOne "index"
  testOne "index2"
  testOne "first"
  testOne "prev"
  testOne "next"
  testOne "fby"
  testOne "ybf"
  testOne "ilog"
  testOne "default"
  testOne "factorial"
  testOne "fibonacci"
  testOne "wvr"
  testOne "primes"
  testOne "ackermann"
  testOne "power"
  testOne "powerF"
  testOne "mm"
  testOne "funCalls"
  testOne "nnext"
  testOne "lexical"

testOne func = do
  textTL <- readFile ("test/tl/" ++ func ++ ".tl")
  textAST <- readFile ("test/ast/" ++ func ++ ".ast")
  textOut <- readFile ("test/out/" ++ func ++ ".out")
  hspec $
    describe func $ do
      it ("tests the " ++ func ++ " function (parse)") $
        processText textTL False True `shouldBe` Right ""
      it ("tests the " ++ func ++ " function (eval)") $
        processText textTL False False `shouldBe` Right textOut
      it ("tests the " ++ func ++ " function (printAST)") $
        processText textTL True True `shouldBe` Right textAST
      it ("tests the " ++ func ++ " function (printAST + eval)") $
        processText textTL True False `shouldBe` Right (textAST ++ textOut)
