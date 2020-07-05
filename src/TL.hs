module TL (
  processText
) where

import TL.Eval
import TL.Parse

processText programText printAST parseOnly =
  do
    let parsedText = parseText programText
    case parsedText of
      Left err -> Left "Parse completely failed"
      Right ans ->
        do
          let outAST = if printAST then (show ans) ++ "\n========\n" else ""
          if not (checkFile ans)
          then Left (outAST ++ "Parse errors, cannot evaluate")
          else Right (outAST ++ (if not parseOnly then evalFile ans else ""))
