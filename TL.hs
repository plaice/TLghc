module TL (
  Ctx, Env,
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
  eval,isDeclDim,isDeclVar,isDeclFunc,isEvalVar,isEvalExpr,
  evalFile
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array

type Ctx = Map.Map Integer TLdata
type Env = Map.Map String TLenv

rank ctx = Map.foldlWithKey (\n k x -> max k n) 0 ctx

findCell a ctx =
  case ord of
    Just i -> i
    Nothing -> error $ "findCell did not find" ++ (show a)
  where ord = Map.lookup a ctx

find s env ctx =
  case dim of
    Just (TLdim a) -> findCell a ctx
    Nothing -> error $ "find did not find " ++ s
  where dim = Map.lookup s env

changeOne s v env ctx =
  case dim of
    Just (TLdim a) -> Map.insert a v ctx
    Nothing -> error $ "changeOne did not find " ++ s
  where dim = Map.lookup s env

change [] env ctx = ctx
change ((s,v):subs) env ctx = changeOne s v env $ change subs env ctx

flatten' set0 env ctx =
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

flatten env ctx = foldl Map.union Map.empty (flatten' Set.empty env ctx)

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
            | TLfunc ([String] -> [TLexpr] -> Env -> Ctx -> TLdata)

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

data TLenv = TLdim Integer
           | TLenvExpr TLexpr
           | TLenvBinding [String] TLexpr Env Ctx

data TLeval = TLevalVar String (Map.Map String TLdata)
            | TLevalExpr TLexpr (Map.Map String TLdata)
  deriving (Show)

isEvalVar (TLevalVar _ _) = True
isEvalVar _               = False

isEvalExpr (TLevalExpr _ _) = True
isEvalExpr _                = False

data TLfile = TLfile [TLdecl] [TLdecl] [TLdecl] [TLeval] [TLeval]
  deriving (Show)

findVar s env =
  case expr of
    Just e -> e
    Nothing -> error $ "findVar did not find " ++ s
  where expr = Map.lookup s env

removeJust (Just (TLint i)) = i

eval :: TLexpr -> Env -> Ctx -> TLdata

eval (TLhash s) env ctx = find s env ctx
eval (TLat args arg) env ctx =
  eval arg env (change (evalPair args env ctx) env ctx)
  where
    evalPair [] env ctx = []
    evalPair ((s,ordinate):args) env ctx =
      (s, eval ordinate env ctx):(evalPair args env ctx)

eval (TLvar s) env ctx =
  case looked of
    TLdim a -> error $ "Dimension used as variable: " ++ s
    TLenvExpr e -> eval e env ctx
    TLenvBinding dims e envBind ctxBind ->
      eval e envBind
           (change (map (\s -> (s, eval (TLhash s) env ctx)) dims)
                        envBind ctxBind)
  where looked = findVar s env

eval (TLwhere dimDecls varDecls funcDecls expr) env ctx =
  eval
    expr
    (Map.union
     (Map.union
      (Map.fromList (map (\(TLdeclDim (s,expr),rk) -> (s, TLdim rk)) triples))
      (Map.union
       (Map.fromList (map (\(TLdeclVar (x,e)) -> (x, TLenvExpr e)) varDecls))
       (Map.fromList (map (\(TLdeclFunc (f,dims,vars,e)) ->
                          (f, TLenvExpr (TLlambda dims vars e))) funcDecls))))
     env)
    (Map.union
     (Map.fromList (map (\(TLdeclDim (s,expr),rk) -> (rk, eval expr env ctx))
                    triples))
     ctx)
  where
    triples = zip dimDecls [1 + rank ctx ..]

eval (TLapply func dimActuals exprActuals) env ctx =
  case funcEval of
    TLfunc funcLambda -> funcLambda dimActuals exprActuals env ctx
    _                 -> error "Type error"
  where funcEval = (eval func env ctx)

eval (TLlambda dimArgs varArgs expr) env ctx =
  TLfunc
  (\dimActuals -> \exprActuals -> \envActuals -> \ctxActuals ->
   let
     maxRank = max (rank ctx) (rank ctxActuals)
     triples = zip (zip dimArgs dimActuals) [1 + maxRank ..]
     pairs = zip varArgs exprActuals
     ctx' = (Map.union
             (Map.fromList (map (\((d,d'),rk) ->
                                (rk, find d' envActuals ctxActuals)) triples))
             ctx)
     envActuals' = (Map.union
                    (Map.fromList (map (\((d,d'),rk) ->
                                       (d', TLdim rk)) triples))
                    envActuals)
     env' = (Map.union
             (Map.union
              (Map.fromList (map (\((d,d'),rk) ->
                                 (d, TLdim rk)) triples))
              (Map.fromList (map (\(x,e) ->
                                 (x, TLenvBinding dimArgs e envActuals'
                                                  ctxActuals)) pairs)))
             env)
   in eval expr env' ctx'
  )

eval (TLconst c) env ctx = c

eval (TLarray1 str arr) env ctx =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten env ctx
     tupleIdx = removeJust $ Map.lookup str flatCtx

eval (TLarray2 (str1,str2) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten env ctx
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx)

eval (TLarray3 (str1,str2,str3) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten env ctx
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx,
                 removeJust $ Map.lookup str3 flatCtx)

eval (TLarray4 (str1,str2,str3,str4) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten env ctx
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx,
                 removeJust $ Map.lookup str3 flatCtx,
                 removeJust $ Map.lookup str4 flatCtx)

eval (TLarray5 (str1,str2,str3,str4,str5) arr) env ctx =
   (Array.!) arr tupleIdx
   where
     flatCtx = flatten env ctx
     tupleIdx = (removeJust $ Map.lookup str1 flatCtx,
                 removeJust $ Map.lookup str2 flatCtx,
                 removeJust $ Map.lookup str3 flatCtx,
                 removeJust $ Map.lookup str4 flatCtx,
                 removeJust $ Map.lookup str5 flatCtx)

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

evalFile (TLfile dims vars funcs evalVars evalExprs) =
  map (\(TLevalExpr e flatCtx) ->
       let (env,ctx) = unflatten flatCtx 0 in
       eval (TLwhere dims vars funcs e) env ctx)
      evalExprs
