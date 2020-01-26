module TL (
  TLctx, TLenv,
  TLdata(TLbool, TLchar, TLint, TLstr, TLfunc),
  TLuno(TLunNot, TLunNegate),
  TLduo(TLbinAnd, TLbinOr,
        TLbinPlus, TLbinMinus, TLbinTimes, TLbinDiv, TLbinMod, TLbinRem,
        TLbinLT, TLbinLE, TLbinGE, TLbinGT, TLbinEQ, TLbinNE),
  TLexpr(TLconst, TLarray1, TLarray2, TLarray3, TLarray4, TLarray5,
         TLunop, TLbinop, TLcond, TLhash, TLat,
         TLvar, TLwhere, TLfn, TLapply),
  TLdecl(TLdeclDim, TLdeclVar, TLdeclFunc),
  TLenvEntry(TLdim, TLenvExpr, TLenvBinding),
  TLeval(TLevalExpr),
  TLfile(TLfile),
  isDeclDim,isDeclVar,isDeclFunc
  ,eval
  ,evalFile
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array

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
  deriving (Show)

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
  deriving (Show)

data TLdecl = TLdeclDim String TLexpr
            | TLdeclVar String TLexpr
            | TLdeclFunc String [String] [String] TLexpr
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
  deriving (Show)

data TLenvEntry = TLdim Integer
                | TLenvExpr TLexpr
                | TLenvBinding [String] TLexpr TLenv TLctx

data TLeval = TLevalExpr TLexpr [(String,(Integer,Integer))]
  deriving (Show)

type TLctx = Map.Map Integer TLdata
type TLenv = Map.Map String TLenvEntry

data TLfile = TLfile [TLdecl] [TLdecl] [TLdecl] [TLeval]
  deriving (Show)

expandRange [] = [[]]
expandRange ((d,(min,max)):ranges) =
 [(d,TLconst (TLint i)):l | i <- [min..max], l <- expandRange ranges]

getDims [] = []
getDims ((d,(min,max)):ranges) =
 (d,TLint 0):(getDims ranges)

evalFile (TLfile dims vars funcs evalExprs) =
  map (\(TLevalExpr expr ctxRange) ->
       let expandedRanges = expandRange ctxRange in
       let externDims = Map.fromList $ getDims ctxRange in
       let (env,ctx) = ctxFromAPI externDims 0 in
       map (\pairs -> eval (TLwhere dims vars funcs (TLat pairs expr)) env ctx)
           expandedRanges)
      evalExprs

eval :: TLexpr -> TLenv -> TLctx -> TLdata

eval (TLhash d) env ctx = ctxLookup d env ctx
eval (TLat args arg) env ctx =
  eval arg env (ctxPerturb (evalPair args env ctx) env ctx)
  where
    evalPair [] env ctx = []
    evalPair ((d,expr):args) env ctx =
      (d, eval expr env ctx):(evalPair args env ctx)

eval (TLvar x) env ctx =
  case envEntry of
    TLdim loc -> error $ "Dimension used as variable: " ++ x
    TLenvExpr expr -> eval expr env ctx
    TLenvBinding dims expr envBind ctxBind ->
      eval expr envBind
           (ctxPerturb (map (\d -> (d, eval (TLhash d) env ctx)) dims)
                       envBind ctxBind)
  where envEntry = envLookup x env

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

eval (TLapply func dimActuals exprActuals) env ctx =
  case funcEval of
    TLfunc funcLambda -> funcLambda dimActuals exprActuals env ctx
    _                 -> error "Type error"
  where funcEval = (eval func env ctx)

eval (TLfn dimArgs varArgs expr) env ctx =
  TLfunc
  (\dimActuals -> \exprActuals -> \envActuals -> \ctxActuals ->
   let
     maxRank = max (ctxRank ctx) (ctxRank ctxActuals)
     triples = zip (zip dimArgs dimActuals) [1 + maxRank ..]
     pairs = zip varArgs exprActuals
     ctxDims = Map.fromList
                 (map (\((d,d'),rk) ->
                      (rk, ctxLookup d' envActuals ctxActuals)) triples)
     ctx' = Map.union ctxDims ctx
     envActualsDims = Map.fromList
                        (map (\((d,d'),rk) -> (d', TLdim rk)) triples)
     envActuals' = (Map.union envActualsDims envActuals)
     envDims = Map.fromList
                 (map (\((d,d'),rk) -> (d, TLdim rk)) triples)
     envBindings = Map.fromList
                     (map (\(x,expr) ->
                           (x, TLenvBinding dimArgs expr
                               envActuals' ctxActuals))
                          pairs)
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
    _            -> error "Type error"
  where val1 = eval arg1 env ctx

eval (TLunop TLunNot arg) env ctx =
  case val of
    TLbool b -> TLbool (not b)
    _        -> error "Type error"
  where val = eval arg env ctx

eval (TLunop TLunNegate arg) env ctx =
  case val of
    TLint i -> TLint (negate i)
    _       -> error "Type error"
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
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

eval (TLbinop TLbinNE    arg1 arg2) env ctx =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (b1 /= b2)
    (TLchar c1, TLchar c2) -> TLbool (c1 /= c2)
    (TLint i1, TLint i2)   -> TLbool (i1 /= i2)
    (TLstr s1, TLstr s2)   -> TLbool (s1 /= s2)
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopBool op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (op b1 b2)
    _                      -> error "Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopInt op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLint (op i1 i2)
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

evalBinopRel op arg1 arg2 env ctx =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLbool (op i1 i2)
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 env ctx, eval arg2 env ctx)

ctxRank ctx = Map.foldlWithKey (\n k x -> max k n) 0 ctx

ctxLookupOrd loc ctx =
  case ord of
    Just v -> v
    Nothing -> error $ "ctxLookupOrd did not find" ++ (show loc)
  where ord = Map.lookup loc ctx

ctxLookup d env ctx =
  case dim of
    Just (TLdim loc) -> ctxLookupOrd loc ctx
    Nothing -> error $ "ctxLookup did not find " ++ d
    _ -> error $ "ctxLookup: " ++ d ++ " is not a dimension identifier"
  where dim = Map.lookup d env

ctxPerturbOne d v env ctx =
  case dim of
    Just (TLdim loc) -> Map.insert loc v ctx
    Nothing -> error $ "ctxPerturbOne did not find " ++ d
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
                          Nothing -> error ("lookAtOne could not find " ++
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
    Nothing -> error $ "envLookup did not find " ++ x
  where expr = Map.lookup x env

isDeclDim (TLdeclDim _ _) = True
isDeclDim _               = False

isDeclVar (TLdeclVar _ _) = True
isDeclVar _               = False

isDeclFunc (TLdeclFunc _ _ _ _) = True
isDeclFunc _                    = False

removeJust (Just (TLint i)) = i
