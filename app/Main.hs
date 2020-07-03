module Main where

import System.IO
import System.Environment
import TL.Eval
import TL.Parse

main :: IO ()
main = do
  args <- getArgs
  let (printAST, parseOnly, files) = readArgs args
  programText <- gather files
  let parsedText = parseText programText
  case parsedText of
    Left err -> error "Parse completely failed"
    Right ans ->
      do if printAST
         then do putStr . show $ ans
                 putStrLn "========"
         else putStr ""
         if checkFile ans
         then putStr ""
         else error "Parse errors, cannot evaluate"
         if not parseOnly 
         then evalFile ans
         else putStr ""


readArgs []  = (False, False, [])
readArgs ("--parseOnly" : "--printAST" : args) = (True, True, args)
readArgs ("--printAST" : "--parseOnly" : args) = (True, True, args)
readArgs ("--printAST" : args) = (True, False, args)
readArgs ("--parseOnly" : args) = (False, True, args)
readArgs args = (False, False, args)

gather []     = getContents
gather fs     = fmap concat $ mapM readFile fs
