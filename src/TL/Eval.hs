{-# LANGUAGE ConstrainedClassMethods #-}

{- |
Module      : TL.Eval
Description : The module for evaluating TransLucid ASTs
Copyright   : (c) John Plaice, 2020
License     : GPL-3
Maintainer  : johnplaice@gmail.com
Stability   : experimental
Portability : Portable
-}
module TL.Eval (
   checkFile
  ,evalFile
  ,eval
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List
import Data.Maybe
import TL.AST

-- | 'checkFile' examines a 'TLfile' for syntax errors
checkFile :: TLfile -> Bool
checkFile (TLfile dims vars funcs evalExprs errs1 errs2 errs3 errs4) =
  null errs1 && null errs2 && null errs3 && null errs4

-- | 'evalFile' evaluates the demands in an AST and generates output
evalFile :: TLfile -> [Char]
evalFile (TLfile dims vars funcs evalExprs errs1 errs2 errs3 errs4) =
  concatMap
   (\(TLevalExpr expr ctxRange) ->
    let expandedRanges = expandRange ctxRange in
    let rangeTexts = removePrefix (map foldPairs expandedRanges) in
    let externDims = Map.fromList $ getDims ctxRange in
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

-- | 'eval' evaluates a TransLucid expression
eval :: TLexpr -> TLenv -> TLctx -> TLdata

eval (TLhash d) env ctx = ctxLookup d env ctx
eval (TLat args arg) env ctx =
  eval arg env (ctxPerturb (evalPair args env ctx) env ctx)
  where
    evalPair [] env ctx = []
    evalPair ((d,expr):args) env ctx =
      (d, eval expr env ctx) : evalPair args env ctx

eval (TLwhere dimDecls varDecls funcDecls expr) env ctx =
  eval expr
       (Map.union (Map.union envDims (Map.union envVars envFuncs)) env)
       (Map.union ctxDims ctx)
  where
    triples  = zip dimDecls [1 + ctxRank ctx ..]
    envDims  = Map.fromList
                 (map (\(TLdeclDim d expr,rk) -> (d, TLdim rk)) triples)
    envVars  = Map.fromList
                 (map (\(TLdeclVar x expr) -> (x, TLenvExpr expr)) varDecls)
    envFuncs = Map.fromList
                 (map (\(TLdeclFunc f dims vars expr) ->
                      (f, TLenvExpr (TLfn dims vars expr))) funcDecls)
    ctxDims  = Map.fromList
                 (map (\(TLdeclDim d expr,rk) -> (rk, eval expr env ctx))
                      triples)

eval (TLvar x) env ctx =
  case envEntry of
    TLdim loc -> error $ "TLvar: Dimension used as variable: " ++ x
    TLenvExpr expr -> eval expr env ctx
    TLenvBinding dimPairs expr envBind ctxBind ->
      eval expr envBind
           (ctxPerturb (map (\(d,d') -> (d', eval (TLhash d) env ctx)) dimPairs)
                       envBind ctxBind)
  where envEntry = envLookup x env

eval (TLapply func dimActuals exprActuals) env ctx =
  case funcEval of
    TLfunc funcLambda -> funcLambda dimActuals exprActuals env ctx
    _                 -> error "TLapply: Type error"
  where funcEval = eval func env ctx

eval (TLfn dimArgs varArgs expr) env ctx =
  TLfunc
  (\dimActuals exprActuals envActuals ctxActuals ->
   let
     maxRank = max (ctxRank ctx) (ctxRank ctxActuals)
     dimPairs = zip dimArgs dimActuals
     dimTriples = zip dimPairs [1 + maxRank ..]
     varPairs = zip varArgs exprActuals
     ctxDims = Map.fromList
                 (map (\((d,d'),rk) ->
                      (rk, ctxLookup d' envActuals ctxActuals)) dimTriples)
     ctx' = Map.union ctxDims ctx
     envDims = Map.fromList
                 (map (\((d,d'),rk) -> (d, TLdim rk)) dimTriples)
     envBindings = Map.fromList
                     (map (\(x,expr) ->
                           (x, TLenvBinding dimPairs expr
                               envActuals ctxActuals))
                          varPairs)
     env' = Map.union (Map.union envDims envBindings) env
   in eval expr env' ctx')

eval (TLconst c) env ctx = c

eval (TLarray1 str arr) env ctx =
   (Array.!) arr tupleIdx
   where
     ctxAPI = ctxToAPI env ctx
     tupleIdx = removeJust $ Map.lookup str ctxAPI

eval (TLarray2 (str1,str2) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     ctxAPI = ctxToAPI env ctx
     tupleIdx = (removeJust $ Map.lookup str1 ctxAPI,
                 removeJust $ Map.lookup str2 ctxAPI)

eval (TLarray3 (str1,str2,str3) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     ctxAPI = ctxToAPI env ctx
     tupleIdx = (removeJust $ Map.lookup str1 ctxAPI,
                 removeJust $ Map.lookup str2 ctxAPI,
                 removeJust $ Map.lookup str3 ctxAPI)

eval (TLarray4 (str1,str2,str3,str4) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     ctxAPI = ctxToAPI env ctx
     tupleIdx = (removeJust $ Map.lookup str1 ctxAPI,
                 removeJust $ Map.lookup str2 ctxAPI,
                 removeJust $ Map.lookup str3 ctxAPI,
                 removeJust $ Map.lookup str4 ctxAPI)

eval (TLarray5 (str1,str2,str3,str4,str5) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     ctxAPI = ctxToAPI env ctx
     tupleIdx = (removeJust $ Map.lookup str1 ctxAPI,
                 removeJust $ Map.lookup str2 ctxAPI,
                 removeJust $ Map.lookup str3 ctxAPI,
                 removeJust $ Map.lookup str4 ctxAPI,
                 removeJust $ Map.lookup str5 ctxAPI)

eval (TLcond arg1 arg2 arg3) env ctx =
  case val1 of
    TLbool True  -> eval arg2 env ctx
    TLbool False -> eval arg3 env ctx
    _            -> error "TLcond: Type error"
  where val1 = eval arg1 env ctx

eval (TLunop TLunNot arg) env ctx =
  case val of
    TLbool b -> TLbool (not b)
    _        -> error "TLunop: Type error"
  where val = eval arg env ctx

eval (TLunop TLunNegate arg) env ctx =
  case val of
    TLint i -> TLint (negate i)
    _       -> error "TLunop: Type error"
  where val = eval arg env ctx

eval (TLbinop TLbinAnd   arg1 arg2) env ctx =
  evalBinopBool (&&) arg1 arg2 env ctx
eval (TLbinop TLbinOr    arg1 arg2) env ctx =
  evalBinopBool (||) arg1 arg2 env ctx

eval (TLbinop TLbinPlus  arg1 arg2) env ctx =
  evalBinopInt  (+)  arg1 arg2 env ctx
eval (TLbinop TLbinMinus arg1 arg2) env ctx =
  evalBinopInt  (-)  arg1 arg2 env ctx
eval (TLbinop TLbinTimes arg1 arg2) env ctx =
  evalBinopInt  (*)  arg1 arg2 env ctx
eval (TLbinop TLbinDiv   arg1 arg2) env ctx =
  evalBinopInt  div  arg1 arg2 env ctx
eval (TLbinop TLbinMod   arg1 arg2) env ctx =
  evalBinopInt  mod  arg1 arg2 env ctx
eval (TLbinop TLbinRem   arg1 arg2) env ctx =
  evalBinopInt  rem  arg1 arg2 env ctx

eval (TLbinop TLbinLT    arg1 arg2) env ctx =
  evalBinopRel  (<)  arg1 arg2 env ctx
eval (TLbinop TLbinLE    arg1 arg2) env ctx =
  evalBinopRel  (<=) arg1 arg2 env ctx
eval (TLbinop TLbinGE    arg1 arg2) env ctx =
  evalBinopRel  (>=) arg1 arg2 env ctx
eval (TLbinop TLbinGT    arg1 arg2) env ctx =
  evalBinopRel  (>)  arg1 arg2 env ctx

eval (TLbinop TLbinEQ    arg1 arg2) env ctx =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (b1 == b2)
    (TLchar c1, TLchar c2) -> TLbool (c1 == c2)
    (TLint i1, TLint i2)   -> TLbool (i1 == i2)
    (TLstr s1, TLstr s2)   -> TLbool (s1 == s2)
    _                    -> error "TLbinop: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

eval (TLbinop TLbinNE    arg1 arg2) env ctx =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (b1 /= b2)
    (TLchar c1, TLchar c2) -> TLbool (c1 /= c2)
    (TLint i1, TLint i2)   -> TLbool (i1 /= i2)
    (TLstr s1, TLstr s2)   -> TLbool (s1 /= s2)
    _                    -> error "TLbinop: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopBool
  :: (Bool -> Bool -> Bool)
     -> TLexpr
     -> TLexpr
     -> Map.Map String TLenvEntry
     -> Map.Map Integer TLdata
     -> TLdata
evalBinopBool op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (op b1 b2)
    _                      -> error "evalBinopBool: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopInt
  :: (Integer -> Integer -> Integer)
     -> TLexpr
     -> TLexpr
     -> Map.Map String TLenvEntry
     -> Map.Map Integer TLdata
     -> TLdata
evalBinopInt op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLint (op i1 i2)
    _                    -> error "evalBinopInt: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopRel
  :: (Integer -> Integer -> Bool)
     -> TLexpr
     -> TLexpr
     -> Map.Map String TLenvEntry
     -> Map.Map Integer TLdata
     -> TLdata
evalBinopRel op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLbool (op i1 i2)
    _                    -> error "evalBinopRel: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

removeJust :: Maybe TLdata -> Integer
removeJust (Just (TLint i)) = i

ctxToAPI'
  :: Ord k =>
     Set.Set k
     -> Map.Map k TLenvEntry
     -> Map.Map Integer a
     -> (Set.Set k, Map.Map k a)
ctxToAPI' set0 env ctx =
  Map.foldlWithKey lookAtOne (set0, Map.fromList []) env
  where
    lookAtOne (set, m) key val
      | Set.member key set = (set, m)
      | otherwise = case val of
                      Just (TLdim i) ->
                        case ord of
                          Just j -> (Set.insert key set, Map.insert key j m)
                          Nothing -> error ("lookAtOne: Did not find " ++
                                            show i)
                        where ord = Map.lookup i ctx
                      _ -> (set, m)
                    where val = Map.lookup key env

ctxToAPI
  :: Ord k =>
     Map.Map k TLenvEntry -> Map.Map Integer a -> Map.Map k a
ctxToAPI env ctx = foldl Map.union Map.empty (ctxToAPI' Set.empty env ctx)

ctxFromAPI'
  :: Ord k =>
     Map.Map k a
     -> Integer -> (Map.Map k TLenvEntry, Map.Map Integer a, Integer)
ctxFromAPI' ctx n =
  Map.foldlWithKey lookAtOne (Map.fromList [], Map.fromList [], n) ctx
  where
    lookAtOne (env', ctx', n') key ord =
      (Map.insert key (TLdim n') env', Map.insert n' ord ctx', n'+1)

ctxFromAPI
  :: Ord k =>
     Map.Map k a -> Integer -> (Map.Map k TLenvEntry, Map.Map Integer a)
ctxFromAPI ctx n = (env', ctx')
  where
    (env', ctx', n') = ctxFromAPI' ctx n

ctxRank :: Map.Map Integer b -> Integer
ctxRank = Map.foldlWithKey (\n k x -> max k n) 0

ctxLookupOrd :: (Show k, Ord k) => k -> Map.Map k a -> a
ctxLookupOrd loc ctx =
  fromMaybe (error $ "ctxLookupOrd: Did not find " ++ show loc) ord
  where ord = Map.lookup loc ctx

ctxLookup
  :: [Char] -> Map.Map [Char] TLenvEntry -> Map.Map Integer p -> p
ctxLookup d env ctx =
  case dim of
    Just (TLdim loc) -> ctxLookupOrd loc ctx
    Nothing -> error $ "ctxLookup: Did not find " ++ d
    _ -> error $ "ctxLookup: " ++ d ++ " is not a dimension identifier"
  where dim = Map.lookup d env

ctxPerturbOne
  :: [Char]
     -> a
     -> Map.Map [Char] TLenvEntry
     -> Map.Map Integer a
     -> Map.Map Integer a
ctxPerturbOne d v env ctx =
  case dim of
    Just (TLdim loc) -> Map.insert loc v ctx
    Nothing -> error $ "ctxPerturbOne: Did not find " ++ d
  where dim = Map.lookup d env

ctxPerturb
  :: [([Char], a)]
     -> Map.Map [Char] TLenvEntry
     -> Map.Map Integer a
     -> Map.Map Integer a
ctxPerturb [] env ctx = ctx
ctxPerturb ((d,v):subs) env ctx =
  ctxPerturbOne d v env $ ctxPerturb subs env ctx

envLookup :: [Char] -> Map.Map [Char] a -> a
envLookup x env =
  fromMaybe (error $ "envLookup: Did not find " ++ x) expr
  where expr = Map.lookup x env

expandRange :: [(a, (Integer, Integer))] -> [[(a, TLexpr)]]
expandRange [] = [[]]
expandRange ((d,(min,max)):ranges) =
 [(d,TLconst (TLint i)):l | i <- [min..max], l <- expandRange ranges]

getDims :: [(a1, (a2, b))] -> [(a1, TLdata)]
getDims [] = []
getDims ((d,(min,max)):ranges) =
 (d,TLint 0) : getDims ranges

foldPairs :: [([Char], TLexpr)] -> [Char]
foldPairs [] = ""
foldPairs [(d,TLconst (TLint i))] =
  d ++ " <- " ++ show i
foldPairs ((d,TLconst (TLint i)):l) =
  d ++ " <- " ++ show i ++ ", " ++ foldPairs l

fixPrefixOne :: Char -> [Char] -> [Char]
fixPrefixOne c [] = " "
fixPrefixOne '-' (' ':pairs) = ' ':' ':pairs
fixPrefixOne '-' (c:pairs) = '-':c:pairs
fixPrefixOne _ (c:pairs) = ' ':c:pairs

removePrefixOne :: [Char] -> [Char] -> [Char]
removePrefixOne [] l = l
removePrefixOne l [] = []
removePrefixOne (c:pairs) (c':pairs')
  | c == c' = let newPairs = removePrefixOne pairs pairs' in
              fixPrefixOne c newPairs
  | otherwise = c':pairs'

removePrefix' :: [Char] -> [[Char]] -> [[Char]]
removePrefix' pairs [] = []
removePrefix' pairs (pairs':l) =
  removePrefixOne pairs pairs' : removePrefix' pairs' l

removePrefix :: [[Char]] -> [[Char]]
removePrefix [] = []
removePrefix (pairs:l) = pairs : removePrefix' pairs l
