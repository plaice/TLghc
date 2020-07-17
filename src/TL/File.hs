{-# LANGUAGE ConstrainedClassMethods #-}

{- |
Module      : TL.File
Description : The module for evaluating a TransLucid program
Copyright   : (c) John Plaice, 2020
License     : GPL-3
Maintainer  : johnplaice@gmail.com
Stability   : experimental
Portability : Portable
-}
module TL.File (
   checkFile
  ,evalFile
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List
import Data.Maybe
import TL.AST
import TL.Eval

-- | 'TLextCtx' is for the external context.
type TLextCtx = [(String,TLdata)]

-- | 'TLextCtxRange' is for the external context range
type TLextCtxRange = [(String,(Integer,Integer))]

-- | 'TLextCtxExpr' is for the external context, as an expression
type TLextCtxExpr = [(String,TLexpr)]

-- | 'checkFile' examines a 'TLfile' for syntax errors.
checkFile :: TLfile -> Bool
checkFile (TLfile dims vars funcs evalExprs errs1 errs2 errs3 errs4) =
  null errs1 && null errs2 && null errs3 && null errs4

-- | 'evalFile' evaluates the demands in an AST and generates output.
evalFile :: TLfile -> String
evalFile (TLfile dims vars funcs evalExprs errs1 errs2 errs3 errs4) =
  concatMap
   (\(TLevalExpr expr extCtxRange) ->
    let expandedRanges = expandCtxRange extCtxRange in
    let rangeTexts = removePrefix (map foldPairs expandedRanges) in
    let externDims = Map.fromList $ getDims extCtxRange in
    let (env,ctx) = ctxFromAPI externDims 0 in
        show expr ++ "\n" ++
        concatMap
         (\(text,pairs) ->
          "  " ++ text ++ ": " ++
          show (eval (TLwhere dims vars funcs (TLat pairs expr))
                     (Map.fromList []) (Map.fromList [])) ++
          "\n")
         (zip rangeTexts expandedRanges))
   evalExprs

-- Utility routines needed for 'evalFile'.

-- | 'expandCtxRange' produces all of the points in a MD-range.
-- The input is a list of '(dimension,(minimum,maximum))'.
-- The output is a list of all the points delimited in this space.
expandCtxRange :: TLextCtxRange -> [TLextCtxExpr]
expandCtxRange [] = [[]]
expandCtxRange ((dim,(min,max)):ranges) =
 [(dim,TLconst (TLconstInt i)):l |
  i <- [min..max], l <- expandCtxRange ranges]

-- | 'getDims' produces all of the points in a MD-range.
getDims :: TLextCtxRange -> TLextCtx
getDims [] = []
getDims ((d,(min,max)):ranges) =
 (d,TLint 0) : getDims ranges

foldPairs :: TLextCtxExpr -> String
foldPairs [] = ""
foldPairs [(d,TLconst (TLconstInt i))] =
  d ++ " <- " ++ show i
foldPairs ((d,TLconst (TLconstInt i)):l) =
  d ++ " <- " ++ show i ++ ", " ++ foldPairs l

fixPrefixOne :: Char -> String -> String
fixPrefixOne c [] = " "
fixPrefixOne '-' (' ':pairs) = ' ':' ':pairs
fixPrefixOne '-' (c:pairs) = '-':c:pairs
fixPrefixOne _ (c:pairs) = ' ':c:pairs

removePrefixOne :: String -> String -> String
removePrefixOne [] l = l
removePrefixOne l [] = []
removePrefixOne (c:pairs) (c':pairs')
  | c == c' = let newPairs = removePrefixOne pairs pairs' in
              fixPrefixOne c newPairs
  | otherwise = c':pairs'

removePrefix' :: String -> [String] -> [String]
removePrefix' pairs [] = []
removePrefix' pairs (pairs':l) =
  removePrefixOne pairs pairs' : removePrefix' pairs' l

removePrefix :: [String] -> [String]
removePrefix [] = []
removePrefix (pairs:l) = pairs : removePrefix' pairs l
