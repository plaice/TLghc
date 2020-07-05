{-# LANGUAGE ConstrainedClassMethods #-}

module TL.Eval (
  TLctx, TLenv,
  TLdata(TLbool, TLchar, TLint, TLstr, TLfunc),
  TLuno(TLunNot, TLunNegate),
  TLduo(TLbinAnd, TLbinOr,
        TLbinPlus, TLbinMinus, TLbinTimes, TLbinDiv, TLbinMod, TLbinRem,
        TLbinLT, TLbinLE, TLbinGE, TLbinGT, TLbinEQ, TLbinNE),
  TLexpr(TLconst, TLarray1, TLarray2, TLarray3, TLarray4, TLarray5,
         TLunop, TLbinop, TLcond, TLhash, TLat,
         TLvar, TLwhere, TLfn, TLapply),
  TLdecl(TLdeclDim, TLdeclDimError,
         TLdeclVar, TLdeclVarError,
         TLdeclFunc),
  TLenvEntry(TLdim, TLenvExpr, TLenvBinding),
  TLeval(TLevalExpr, TLevalExprError),
  TLfile(TLfile),
  isDeclDim,isDeclVar,isDeclFunc
  ,isDeclDimError,isDeclVarError
  ,isEvalExpr,isEvalExprError
  ,eval
  ,checkFile
  ,evalFile
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List

data TLdata = TLbool Bool
            | TLchar Char
            | TLint Integer
            | TLstr String
            | TLfunc ([String] -> [TLexpr] -> TLenv -> TLctx -> TLdata)

instance Show TLdata where
  show (TLbool b) = "TLbool " ++ (show b)
  show (TLchar c) = "TLchar " ++ (show c)
  show (TLint i) = "TLint " ++ (show i)
  show (TLstr s) = "TLstr " ++ (show s)
  show (TLfunc f) = "TLfunc"

data TLuno = TLunNot
           | TLunNegate
  deriving Show

data TLduo = TLbinAnd
           | TLbinOr
           | TLbinPlus
           | TLbinMinus
           | TLbinTimes
           | TLbinDiv
           | TLbinMod
           | TLbinRem
           | TLbinLT
           | TLbinLE
           | TLbinGE
           | TLbinGT
           | TLbinEQ
           | TLbinNE
  deriving Show

data TLexpr = TLconst TLdata
            | TLarray1 String
                       (Array.Array
                        Integer
                        TLdata)
            | TLarray2 (String,String)
                       (Array.Array
                        (Integer,Integer)
                        TLdata)
            | TLarray3 (String,String,String)
                       (Array.Array
                        (Integer,Integer,Integer)
                        TLdata)
            | TLarray4 (String,String,String,String)
                       (Array.Array
                        (Integer,Integer,Integer,Integer)
                        TLdata)
            | TLarray5 (String,String,String,String,String)
                       (Array.Array
                        (Integer,Integer,Integer,Integer,Integer)
                        TLdata)
            | TLunop TLuno TLexpr
            | TLbinop TLduo TLexpr TLexpr
            | TLcond TLexpr TLexpr TLexpr
            | TLhash String
            | TLat [(String, TLexpr)] TLexpr
            | TLvar String
            | TLwhere [TLdecl] [TLdecl] [TLdecl] TLexpr
            | TLfn [String] [String] TLexpr
            | TLapply TLexpr [String] [TLexpr]
  deriving Show

showN 0 = ""
showN n = " " ++ showN (n-1)

instance Show' TLexpr where
  show' a@(TLconst _) n = show a
  show' a@(TLarray1 _ _) n = show a
  show' a@(TLarray2 _ _) n = show a
  show' a@(TLarray3 _ _) n = show a
  show' a@(TLarray4 _ _) n = show a
  show' a@(TLarray5 _ _) n = show a
  show' (TLunop unop arg) n =
    "TLunop " ++ show unop ++
    "\n" ++ showN (n+2) ++
    show' arg (n+2)
  show' (TLbinop binop lhs rhs) n =
    "TLbinop " ++ show binop ++
    "\n" ++ showN (n+2) ++
    show' lhs (n+2) ++
    "\n" ++ showN (n+2) ++
    show' rhs (n+2)
  show' (TLcond ifExpr thenExpr elseExpr) n =
    "TLcond" ++
    "\n" ++ showN (n+2) ++
    show' ifExpr (n+2) ++
    "\n" ++ showN (n+2) ++
    show' thenExpr (n+2) ++
    "\n" ++ showN (n+2) ++
    show' elseExpr (n+2)
  show' a@(TLhash _) n = show a
  show' (TLat pairs expr) n =
    "TLat\n" ++
    (concat $
     map (\(dim,exp) ->
          showN (n+2) ++ show dim ++ "\n" ++
          showN (n+2) ++ show' exp (n+2) ++ "\n") pairs) ++
    showN (n+2) ++
    show' expr (n+2)
  show' a@(TLvar _) n = show a
  show' (TLwhere dims vars funcs expr) n =
    "TLwhere\n" ++
    (concat $
     (filter (\l -> not (null l))
      [concat $ (map (\act -> show' act (n+2)) dims),
       concat $ (map (\act -> show' act (n+2)) vars),
       concat $ (map (\act -> show' act (n+2)) funcs)])) ++
    showN (n+2) ++
    show' expr (n+2)
  show' (TLfn dimArgs varArgs expr) n =
    "TLfn" ++
    "\n" ++ showN (n+2) ++
    show dimArgs ++
    "\n" ++ showN (n+2) ++
    show varArgs ++
    "\n" ++ showN (n+2) ++
     show' expr (n+2)
  show' (TLapply fnActual dimActuals exprActuals) n =
    "TLapply " ++ show fnActual ++ "\n" ++
    (concat $ intersperse "\n"
     (filter (\l -> not (null l))
      [concat $ intersperse "\n" (map (\act -> showN (n+2) ++ show act) dimActuals),
       concat $ intersperse "\n" (map (\act -> showN (n+2) ++ show' act (n+2)) exprActuals)]))


data TLenvEntry = TLdim Integer
                | TLenvExpr TLexpr
                | TLenvBinding [(String,String)] TLexpr TLenv TLctx
  deriving Show

class Show' a where
  show' :: (Show a) => a -> Int -> String
  show' arg n = show arg

data TLeval = TLevalExpr TLexpr [(String,(Integer,Integer))]
            | TLevalExprError String
  deriving Show

instance Show' TLeval where
  show' (TLevalExpr expr ranges) n =
    showN n ++
    "TLevalExpr " ++ show' expr (n+2) ++
    "\n" ++ showN (n+2) ++
    show ranges ++ "\n"
  show' (TLevalExprError err) n =
    showN n ++
    "TLevalExprError " ++ show err ++ "\n"

type TLctx = Map.Map Integer TLdata
type TLenv = Map.Map String TLenvEntry

data TLdecl = TLdeclDim String TLexpr
            | TLdeclDimError String
            | TLdeclVar String TLexpr
            | TLdeclVarError String
            | TLdeclFunc String [String] [String] TLexpr
  deriving Show

instance Show' TLdecl where
  show' (TLdeclDim name expr) n =
    showN n ++
    "TLdeclDim " ++ show name ++
    "\n" ++ showN (n+2) ++
    show' expr (n+2) ++ "\n"
  show' (TLdeclDimError err) n =
    showN n ++
    "TLdeclDimError " ++ show err ++ "\n"
  show' (TLdeclVar name expr) n =
    showN n ++
    "TLdeclVar " ++ show name ++
    "\n" ++ showN (n+2) ++
    show' expr (n+2) ++ "\n"
  show' (TLdeclVarError err) n =
    showN n ++
    "TLdeclVarError " ++ show err ++ "\n"
  show' (TLdeclFunc name dimArgs varArgs expr) n =
    showN n ++
    "TLdeclFunc " ++ show name ++
    " " ++ show dimArgs ++
    " " ++ show varArgs ++
    "\n" ++ showN (n+2) ++
    show' expr (n+2) ++ "\n"

data TLfile = TLfile [TLdecl] [TLdecl] [TLdecl] [TLeval]
                     [TLdecl] [TLdecl] [TLeval]

instance Show TLfile where
  show (TLfile dims vars funcs evalExprs errs1 errs2 errs3) =
    "TLfile\n" ++
    (concat $ map (\dim -> show' dim 2) dims) ++
    (concat $ map (\var -> show' var 2) vars) ++
    (concat $ map (\func -> show' func 2) funcs) ++
    (concat $ map (\evalExpr -> show' evalExpr 2) evalExprs) ++
    (concat $ map (\err1 -> show' err1 2) errs1) ++
    (concat $ map (\err2 -> show' err2 2) errs2) ++
    (concat $ map (\err3 -> show' err3 2) errs3)

expandRange [] = [[]]
expandRange ((d,(min,max)):ranges) =
 [(d,TLconst (TLint i)):l | i <- [min..max], l <- expandRange ranges]

getDims [] = []
getDims ((d,(min,max)):ranges) =
 (d,TLint 0):(getDims ranges)

foldPairs [] = ""
foldPairs ((d,TLconst (TLint i)):[]) = d ++ " <- " ++ (show i)
foldPairs ((d,TLconst (TLint i)):l) = d ++ " <- " ++ (show i) ++ ", " ++ (foldPairs l)

fixPrefixOne c [] = " "
fixPrefixOne '-' (' ':pairs) = ' ':' ':pairs
fixPrefixOne '-' (c:pairs) = '-':c:pairs
fixPrefixOne _ (c:pairs) = ' ':c:pairs

removePrefixOne [] l = l
removePrefixOne l [] = []
removePrefixOne (c:pairs) (c':pairs')
  | c == c' = let newPairs = removePrefixOne pairs pairs' in
              fixPrefixOne c newPairs
  | otherwise = (c':pairs')

removePrefix' pairs [] = []
removePrefix' pairs (pairs':l) = (removePrefixOne pairs pairs'):(removePrefix' pairs' l)

removePrefix [] = []
removePrefix (pairs:l) = pairs:(removePrefix' pairs l)

checkFile (TLfile dims vars funcs evalExprs errs1 errs2 errs3) =
  (length errs1 == 0) && (length errs2 == 0) && (length errs3 == 0)

evalFile (TLfile dims vars funcs evalExprs errs1 errs2 errs3) =
  concat
  (map (\(TLevalExpr expr ctxRange) ->
        let expandedRanges = expandRange ctxRange in
        let rangeTexts = removePrefix (map foldPairs expandedRanges) in
        let externDims = Map.fromList $ getDims ctxRange in
        let (env,ctx) = ctxFromAPI externDims 0 in
        (show expr) ++ "\n" ++ (concat
        (map (\(text,pairs) ->
              ("  " ++ (id text) ++ ": " ++
               (show (eval (TLwhere dims vars funcs (TLat pairs expr))
                           (Map.fromList []) (Map.fromList []))) ++
               "\n"))
            (zip rangeTexts expandedRanges))))
       evalExprs)

eval :: TLexpr -> TLenv -> TLctx -> TLdata

eval (TLhash d) env ctx = ctxLookup d env ctx
eval (TLat args arg) env ctx =
  eval arg env (ctxPerturb (evalPair args env ctx) env ctx)
  where
    evalPair [] env ctx = []
    evalPair ((d,expr):args) env ctx =
      (d, eval expr env ctx):(evalPair args env ctx)

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
  where funcEval = (eval func env ctx)

eval (TLfn dimArgs varArgs expr) env ctx =
  TLfunc
  (\dimActuals -> \exprActuals -> \envActuals -> \ctxActuals ->
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
     env' = (Map.union (Map.union envDims envBindings) env)
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

evalBinopBool op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (op b1 b2)
    _                      -> error "evalBinopBool: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopInt op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLint (op i1 i2)
    _                    -> error "evalBinopInt: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopRel op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLbool (op i1 i2)
    _                    -> error "evalBinopRel: Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

ctxRank ctx = Map.foldlWithKey (\n k x -> max k n) 0 ctx

ctxLookupOrd loc ctx =
  case ord of
    Just v -> v
    Nothing -> error $ "ctxLookupOrd: Did not find " ++ (show loc)
  where ord = Map.lookup loc ctx

ctxLookup d env ctx =
  case dim of
    Just (TLdim loc) -> ctxLookupOrd loc ctx
    Nothing -> error $ "ctxLookup: Did not find " ++ d
    _ -> error $ "ctxLookup: " ++ d ++ " is not a dimension identifier"
  where dim = Map.lookup d env

ctxPerturbOne d v env ctx =
  case dim of
    Just (TLdim loc) -> Map.insert loc v ctx
    Nothing -> error $ "ctxPerturbOne: Did not find " ++ d
  where dim = Map.lookup d env

ctxPerturb [] env ctx = ctx
ctxPerturb ((d,v):subs) env ctx =
  ctxPerturbOne d v env $ ctxPerturb subs env ctx

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
                                            (show i))
                        where ord = Map.lookup i ctx
                      _ -> (set, m)
                    where val = Map.lookup key env

ctxToAPI env ctx = foldl Map.union Map.empty (ctxToAPI' Set.empty env ctx)

ctxFromAPI' ctx n =
  Map.foldlWithKey lookAtOne (Map.fromList [], Map.fromList [], n) ctx
  where
    lookAtOne (env', ctx', n') key ord =
      (Map.insert key (TLdim n') env', Map.insert n' ord ctx', n'+1)

ctxFromAPI ctx n = (env', ctx')
  where
    (env', ctx', n') = ctxFromAPI' ctx n

envLookup x env =
  case expr of
    Just e -> e
    Nothing -> error $ "envLookup: Did not find " ++ x
  where expr = Map.lookup x env

isDeclDim (TLdeclDim _ _) = True
isDeclDim _               = False

isDeclVar (TLdeclVar _ _) = True
isDeclVar _               = False

isDeclFunc (TLdeclFunc _ _ _ _) = True
isDeclFunc _                    = False

isDeclDimError (TLdeclDimError _) = True
isDeclDimError _                  = False

isDeclVarError (TLdeclVarError _) = True
isDeclVarError _                  = False

isEvalExpr (TLevalExpr _ _) = True
isEvalExpr _                = False

isEvalExprError (TLevalExprError _) = True
isEvalExprError _                   = False

removeJust (Just (TLint i)) = i
