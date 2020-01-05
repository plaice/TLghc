module TL (
  CtxStack,
  TLdata(TLbool, TLchar, TLint, TLstr, TLfunc),
  TLuno(TLunNot, TLunNegate),
  TLduo(TLbinAnd, TLbinOr,
        TLbinPlus, TLbinMinus, TLbinTimes, TLbinDiv, TLbinMod, TLbinRem,
        TLbinLT, TLbinLE, TLbinGE, TLbinGT, TLbinEQ, TLbinNE),
  TLexpr(TLconst, TLarray1, TLarray2, TLarray3, TLarray4, TLarray5,
         TLunop, TLbinop, TLcond, TLhash, TLat,
         TLvar, TLwhere, TLlambda, TLapply),
  TLdecl(TLdeclDim, TLdeclVar, TLdeclFunc),
  TLenv(TLdim, TLenvExpr, TLenvBinding),
  TLeval(TLevalVar,TLevalExpr),
  TLfile(TLfile),
  eval,evalFile,isDeclDim,isDeclVar,isDeclFunc,isEvalVar,isEvalExpr
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array

type CtxStack a = [Map.Map Integer a]

rank ctxs = foldl max 0 $
            map (\ctx -> Map.foldlWithKey (\n k x -> max k n) 0 ctx) ctxs

findCell a [] = error $ "findCell did not find" ++ (show a)
findCell a (ctx:ctxRest) =
  case ord of
    Just i -> i
    Nothing -> findCell a ctxRest
  where ord = Map.lookup a ctx

-- Not a complete case.
find s [] _ = error $ "find did not find " ++ s
find s _ [] = error $ "find did not find " ++ s
find s (env:envRest) (ctxs@(ctx:ctxRest)) =
  case dim of
    Just (TLdim a) -> findCell a ctxs
    Nothing -> find s envRest ctxRest
  where dim = Map.lookup s env

findDim s [] = error $ "findDim did not find " ++ s
findDim s (env:envRest) =
  case dim of
    Just (TLdim a) -> TLdim a
    Nothing -> findDim s envRest
  where dim = Map.lookup s env

changeCell a v [] = []
changeCell a v (ctx:ctxRest) =
  case ord of
    Just i -> (Map.insert a v ctx):ctxRest
    Nothing -> ctx:(changeCell a v ctxRest)
  where ord = Map.lookup a ctx

changeOne s v [] _ = []
changeOne s v _ [] = []
changeOne s v (env:envRest) (ctxs@(ctx:ctxRest)) =
  case dim of
    Just (TLdim a) -> changeCell a v ctxs
    Nothing -> ctx:(changeOne s v envRest ctxRest)
  where dim = Map.lookup s env

change [] envs ctxs = ctxs
change ((s,v):subs) envs ctxs = changeOne s v envs $ change subs envs ctxs

replaceOneDim (set, m) (dim,dimActuals) envActual =
  case ord of
    Just (TLdim a) -> (Set.insert dim set, Map.insert dim (TLdim a) m)
    _ -> (set, m)
  where ord = Map.lookup dimActuals envActual

replaceOne (set, m) dimPairs envActual =
  foldl (\(set', m') (dim, dimActuals) ->
         replaceOneDim (set', m') (dim, dimActuals) envActual)
        (set, m) dimPairs

replace' set dimPairs [] = []
replace' set dimPairs (env:envs) = (set',m'):(replace' set' dimPairs envs)
  where
    (set',m') = replaceOne (set, Map.fromList []) dimPairs env

replace dimPairs envActuals =
  map snd $ replace' Set.empty dimPairs envActuals

flattenOne set0 env ctx =
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

flatten' set [] _ = []
flatten' set _ [] = []
flatten' set (env:envs) (ctx:ctxs) = ctx':(flatten' set' envs ctxs)
  where
    (set',ctx') = flattenOne set env ctx

flatten envs ctxs = foldl Map.union Map.empty (flatten' Set.empty envs ctxs)

unflatten' ctx n =
  Map.foldlWithKey lookAtOne (Map.fromList [], Map.fromList [], n) ctx
  where
    lookAtOne (env', ctx', n') key ord =
      (Map.insert key (TLdim n') env', Map.insert n' ord ctx', n'+1)

unflatten ctx n = (env', ctx')
  where
    (env', ctx', n') = unflatten' ctx n

data TLdata = TLbool Bool
            | TLchar Char
            | TLint Integer
            | TLstr String
            | TLfunc ([String] -> [TLexpr] -> [Map.Map String TLenv]
                               -> (CtxStack TLdata) -> TLdata)

instance Show TLdata where
  show (TLbool b) = "TLbool " ++ (show b)
  show (TLchar c) = "TLchar " ++ (show c)
  show (TLint i) = "TLint " ++ (show i)
  show (TLstr s) = "TLstr " ++ (show s)
  show (TLfunc f) = "TLfunc"

data TLdecl = TLdeclDim (String, TLexpr)
            | TLdeclVar (String, TLexpr)
            | TLdeclFunc (String, [String], [String], TLexpr)
  deriving Show

isDeclDim (TLdeclDim _) = True
isDeclDim _             = False

isDeclVar (TLdeclVar _) = True
isDeclVar _             = False

isDeclFunc (TLdeclFunc _) = True
isDeclFunc _              = False

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
            | TLlambda [String] [String] TLexpr
            | TLapply TLexpr [String] [TLexpr]
  deriving (Show)

-- I need to integrate ordinary Haskell MD arrays as input.
-- Do I need to integrate demand-driven intensional identifiers too?

data TLenv = TLdim Integer
           | TLenvExpr TLexpr
           | TLenvBinding TLexpr [Map.Map String TLenv]

data TLeval = TLevalVar String (Map.Map String TLdata)
            | TLevalExpr TLexpr (Map.Map String TLdata)
  deriving (Show)

isEvalVar (TLevalVar _ _) = True
isEvalVar _               = False

isEvalExpr (TLevalExpr _ _) = True
isEvalExpr _                = False

data TLfile = TLfile [TLdecl] [TLdecl] [TLdecl] [TLeval] [TLeval]
  deriving (Show)

findVar s [] = error $ "find' did not find " ++ s
findVar s (env:envRest) =
  case expr of
    Just e -> e
    Nothing -> findVar s envRest
  where expr = Map.lookup s env

removeJust (Just (TLint i)) = i

eval :: TLexpr -> [Map.Map String TLenv] -> (CtxStack TLdata) -> TLdata

eval (TLvar s) envs ctxs =
  case looked of
    TLdim a -> error $ "Dimension used as variable: " ++ s
    TLenvExpr e -> eval e envs ctxs
    TLenvBinding e envBind -> eval e envBind ctxs
  where looked = findVar s envs

eval (TLwhere dimDecls varDecls funcDecls expr) envs ctxs =
  eval
    expr
    ((Map.union
      (Map.fromList (map (\(TLdeclDim (s,expr),rk) -> (s, TLdim rk)) triples))
      (Map.union
       (Map.fromList (map (\(TLdeclVar (x,e)) -> (x, TLenvExpr e)) varDecls))
       (Map.fromList (map (\(TLdeclFunc (f,dims,vars,e)) ->
                          (f, TLenvExpr (TLlambda dims vars e))) funcDecls))))
     :envs)
    ((Map.fromList (map (\(TLdeclDim (s,expr),rk) -> (rk, eval expr envs ctxs))
                    triples))
     :ctxs)
  where
    triples = zip dimDecls [1 + rank ctxs ..]

eval (TLapply func dimActuals exprActuals) envs ctxs =
  case funcEval of
    TLfunc funcLambda -> funcLambda dimActuals exprActuals envs ctxs
    _                 -> error "Type error"
  where funcEval = (eval func envs ctxs)

eval (TLlambda dimArgs varArgs expr) envs ctxs =
  TLfunc
  (\dimActuals -> \exprActuals -> \envActuals -> \ctxActuals ->
   let
     envActuals' = replace (zip dimArgs dimActuals) envActuals
     envActualHead' =
        Map.union
          (head envActuals')
          (Map.fromList (zip varArgs
                         (map (\e -> TLenvBinding e envActuals) exprActuals)))
     envActuals'' = envActualHead':(tail envActuals')
   in eval expr (envActuals'' ++ envs) (ctxActuals ++ ctxs)
  )

eval (TLhash s) envs ctxs = find s envs ctxs
eval (TLat args arg) envs ctxs =
  eval arg envs (change (evalPair args envs ctxs) envs ctxs)
  where
    evalPair [] envs ctxs = []
    evalPair ((s,ordinate):args) envs ctxs =
      (s, eval ordinate envs ctxs):(evalPair args envs ctxs)

eval (TLconst c) envs ctxs = c

eval (TLarray1 str arr) envs ctxs =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten envs ctxs
     tupleIdx = removeJust $ Map.lookup str flatCtx

eval (TLarray2 (str1,str2) arr) envs ctxs =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten envs ctxs
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx)

eval (TLarray3 (str1,str2,str3) arr) envs ctxs =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten envs ctxs
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx,
                 removeJust $ Map.lookup str3 flatCtx)

eval (TLarray4 (str1,str2,str3,str4) arr) envs ctxs =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten envs ctxs
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx,
                 removeJust $ Map.lookup str3 flatCtx,
                 removeJust $ Map.lookup str4 flatCtx)

eval (TLarray5 (str1,str2,str3,str4,str5) arr) envs ctxs =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten envs ctxs
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx,
                 removeJust $ Map.lookup str3 flatCtx,
                 removeJust $ Map.lookup str4 flatCtx,
                 removeJust $ Map.lookup str5 flatCtx)

eval (TLcond arg1 arg2 arg3) envs ctxs =
  case val1 of
    TLbool True  -> eval arg2 envs ctxs
    TLbool False -> eval arg3 envs ctxs
    _            -> error "Type error"
  where val1 = eval arg1 envs ctxs

eval (TLunop TLunNot arg) envs ctxs =
  case val of
    TLbool b -> TLbool (not b)
    _        -> error "Type error"
  where val = eval arg envs ctxs

eval (TLunop TLunNegate arg) envs ctxs =
  case val of
    TLint i -> TLint (negate i)
    _       -> error "Type error"
  where val = eval arg envs ctxs

eval (TLbinop TLbinAnd   arg1 arg2) envs ctxs =
  evalBinopBool (&&) arg1 arg2 envs ctxs
eval (TLbinop TLbinOr    arg1 arg2) envs ctxs =
  evalBinopBool (||) arg1 arg2 envs ctxs

eval (TLbinop TLbinPlus  arg1 arg2) envs ctxs =
  evalBinopInt  (+)  arg1 arg2 envs ctxs
eval (TLbinop TLbinMinus arg1 arg2) envs ctxs =
  evalBinopInt  (-)  arg1 arg2 envs ctxs
eval (TLbinop TLbinTimes arg1 arg2) envs ctxs =
  evalBinopInt  (*)  arg1 arg2 envs ctxs
eval (TLbinop TLbinDiv   arg1 arg2) envs ctxs =
  evalBinopInt  div  arg1 arg2 envs ctxs
eval (TLbinop TLbinMod   arg1 arg2) envs ctxs =
  evalBinopInt  mod  arg1 arg2 envs ctxs
eval (TLbinop TLbinRem   arg1 arg2) envs ctxs =
  evalBinopInt  rem  arg1 arg2 envs ctxs

eval (TLbinop TLbinLT    arg1 arg2) envs ctxs =
  evalBinopRel  (<)  arg1 arg2 envs ctxs
eval (TLbinop TLbinLE    arg1 arg2) envs ctxs =
  evalBinopRel  (<=) arg1 arg2 envs ctxs
eval (TLbinop TLbinGE    arg1 arg2) envs ctxs =
  evalBinopRel  (>=) arg1 arg2 envs ctxs
eval (TLbinop TLbinGT    arg1 arg2) envs ctxs =
  evalBinopRel  (>)  arg1 arg2 envs ctxs

eval (TLbinop TLbinEQ    arg1 arg2) envs ctxs =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (b1 == b2)
    (TLchar c1, TLchar c2) -> TLbool (c1 == c2)
    (TLint i1, TLint i2)   -> TLbool (i1 == i2)
    (TLstr s1, TLstr s2)   -> TLbool (s1 == s2)
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 envs ctxs, eval arg2 envs ctxs)

eval (TLbinop TLbinNE    arg1 arg2) envs ctxs =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (b1 /= b2)
    (TLchar c1, TLchar c2) -> TLbool (c1 /= c2)
    (TLint i1, TLint i2)   -> TLbool (i1 /= i2)
    (TLstr s1, TLstr s2)   -> TLbool (s1 /= s2)
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 envs ctxs, eval arg2 envs ctxs)

evalBinopBool op arg1 arg2 envs ctxs =
  case (val1, val2) of
    (TLbool b1, TLbool b2) -> TLbool (op b1 b2)
    _                      -> error "Type error"
  where (val1, val2) = (eval arg1 envs ctxs, eval arg2 envs ctxs)

evalBinopInt op arg1 arg2 envs ctxs =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLint (op i1 i2)
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 envs ctxs, eval arg2 envs ctxs)

evalBinopRel op arg1 arg2 envs ctxs =
  case (val1, val2) of
    (TLint i1, TLint i2) -> TLbool (op i1 i2)
    _                    -> error "Type error"
  where (val1, val2) = (eval arg1 envs ctxs, eval arg2 envs ctxs)

evalFile (TLfile dims vars funcs evalVars evalExprs) =
  map (\(TLevalExpr e flatCtx) ->
       let (env,ctx) = unflatten flatCtx 0 in
       eval (TLwhere dims vars funcs e) [env] [ctx])
      evalExprs
