{- |
Module      : TL
Description : The top level TLghc module
Copyright   : (c) John Plaice, 2020
License     : GPL-3
Maintainer  : johnplaice@gmail.com
Stability   : experimental
Portability : Portable
-}
module TL (
  processText
) where

import TL.Eval
import TL.Parse

-- | The 'processText' function processes a TransLucid program.
-- The input is parsed, then evaluated, then the output is generated.
-- * 'programText' is the text of the program.
-- * 'printAST', if True, prints out the resulting AST.
-- * 'parseOnly', if True, only parses the input.
processText :: [Char] -> Bool -> Bool -> Either [Char] [Char]
processText programText printAST parseOnly =
  do
    let parsedText = parseText programText
    case parsedText of
      Left err -> Left "Parse completely failed"
      Right ans ->
        do
          let outAST = if printAST then show ans ++ "========\n" else ""
          if not (checkFile ans)
          then Left (outAST ++ "Parse errors, cannot evaluate")
          else Right (outAST ++ (if not parseOnly then evalFile ans else ""))
