{-# LANGUAGE ConstrainedClassMethods #-}

module TL.AST (
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
