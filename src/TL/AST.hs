{-# LANGUAGE ConstrainedClassMethods #-}

{- |
Module      : TL.AST
Description : The module for TransLucid ASTs
Copyright   : (c) John Plaice, 2020
License     : GPL-3
Maintainer  : johnplaice@gmail.com
Stability   : experimental
Portability : Portable
-}
module TL.AST (
   TLfile(..)
  ,TLdecl(..)
  ,TLexpr(..)
  ,TLconstData(..)
  ,TLduo(..)
  ,TLuno(..)
  ,TLeval(..)
  ,TLextCtxRange(..)
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List

-- | 'TLfile' is the AST for a TransLucid program.
data TLfile
  = TLfile [TLdecl] [TLdecl] [TLdecl] [TLeval]
           [TLdecl] [TLdecl] [TLdecl] [TLeval]

instance Show TLfile where
  show (TLfile dims vars funcs evalExprs errs1 errs2 errs3 errs4) =
    "TLfile\n" ++
    concatMap (`showN` 2) dims ++
    concatMap (`showN` 2) vars ++
    concatMap (`showN` 2) funcs ++
    concatMap (`showN` 2) evalExprs ++
    concatMap (`showN` 2) errs1 ++
    concatMap (`showN` 2) errs2 ++
    concatMap (`showN` 2) errs3 ++
    concatMap (`showN` 2) errs4

-- | 'TLdecl' is the AST for a TransLucid declaration.
data TLdecl
  = TLdeclDim String TLexpr                     -- ^ dimension identifier
  | TLdeclDimError String                       -- ^ syntax error in dim decl
  | TLdeclVar String TLexpr                     -- ^ variable identifier
  | TLdeclVarError String                       -- ^ syntax error in var decl
  | TLdeclFunc String [String] [String] TLexpr  -- ^ function identifier
  | TLdeclFunError String                       -- ^ syntax error in fun decl
    deriving Show

-- | 'indentN' generates 'n' blanks.
indentN :: Int -> String
indentN 0 = ""
indentN n = " " ++ indentN (n-1)

-- | 'ShowN' class for 'Show' with indentation.
class ShowN a where
  showN :: (Show a) => a -> Int -> String
  showN arg n = show arg

instance ShowN TLdecl where
  showN (TLdeclDim name expr) n =
    indentN n ++
    "TLdeclDim " ++ show name ++
    "\n" ++ indentN (n+2) ++
    showN expr (n+2) ++ "\n"
  showN (TLdeclDimError err) n =
    indentN n ++
    "TLdeclDimError " ++ show err ++ "\n"
  showN (TLdeclVar name expr) n =
    indentN n ++
    "TLdeclVar " ++ show name ++
    "\n" ++ indentN (n+2) ++
    showN expr (n+2) ++ "\n"
  showN (TLdeclVarError err) n =
    indentN n ++
    "TLdeclVarError " ++ show err ++ "\n"
  showN (TLdeclFunc name dimArgs varArgs expr) n =
    indentN n ++
    "TLdeclFunc " ++ show name ++
    " " ++ show dimArgs ++
    " " ++ show varArgs ++
    "\n" ++ indentN (n+2) ++
    showN expr (n+2) ++ "\n"
  showN (TLdeclFunError err) n =
    indentN n ++
    "TLdeclFunError " ++ show err ++ "\n"

-- | 'TLexpr' is the AST for a TransLucid expression.
data TLexpr
  = TLconst TLconstData
  | TLarray1 String
             (Array.Array Integer TLconstData)
  | TLarray2 (String,String)
             (Array.Array (Integer,Integer) TLconstData)
  | TLarray3 (String,String,String)
             (Array.Array (Integer,Integer,Integer) TLconstData)
  | TLarray4 (String,String,String,String)
             (Array.Array (Integer,Integer,Integer,Integer) TLconstData)
  | TLarray5 (String,String,String,String,String)
             (Array.Array (Integer,Integer,Integer,Integer,Integer) TLconstData)
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

instance ShowN TLexpr where
  showN a@(TLconst _) n = show a
  showN a@(TLarray1 _ _) n = show a
  showN a@(TLarray2 _ _) n = show a
  showN a@(TLarray3 _ _) n = show a
  showN a@(TLarray4 _ _) n = show a
  showN a@(TLarray5 _ _) n = show a
  showN (TLunop unop arg) n =
    "TLunop " ++ show unop ++
    "\n" ++ indentN (n+2) ++
    showN arg (n+2)
  showN (TLbinop binop lhs rhs) n =
    "TLbinop " ++ show binop ++
    "\n" ++ indentN (n+2) ++
    showN lhs (n+2) ++
    "\n" ++ indentN (n+2) ++
    showN rhs (n+2)
  showN (TLcond ifExpr thenExpr elseExpr) n =
    "TLcond" ++
    "\n" ++ indentN (n+2) ++
    showN ifExpr (n+2) ++
    "\n" ++ indentN (n+2) ++
    showN thenExpr (n+2) ++
    "\n" ++ indentN (n+2) ++
    showN elseExpr (n+2)
  showN a@(TLhash _) n = show a
  showN (TLat pairs expr) n =
    "TLat\n" ++
    concatMap
     (\(dim,exp) ->
      indentN (n+2) ++ show dim ++ "\n" ++
      indentN (n+2) ++ showN exp (n+2) ++ "\n") pairs ++
    indentN (n+2) ++
    showN expr (n+2)
  showN a@(TLvar _) n = show a
  showN (TLwhere dims vars funcs expr) n =
    "TLwhere\n" ++
    concat
     (filter (not . null)
      [concatMap (\act -> showN act (n+2)) dims,
       concatMap (\act -> showN act (n+2)) vars,
       concatMap (\act -> showN act (n+2)) funcs]) ++
    indentN (n+2) ++
    showN expr (n+2)
  showN (TLfn dimArgs varArgs expr) n =
    "TLfn" ++
    "\n" ++ indentN (n+2) ++
    show dimArgs ++
    "\n" ++ indentN (n+2) ++
    show varArgs ++
    "\n" ++ indentN (n+2) ++
     showN expr (n+2)
  showN (TLapply fnActual dimActuals exprActuals) n =
    "TLapply " ++ show fnActual ++ "\n" ++
    intercalate "\n"
     (filter (not . null)
      [intercalate "\n"
         (map (\act -> indentN (n+2) ++ show act) dimActuals),
       intercalate "\n"
         (map (\act -> indentN (n+2) ++ showN act (n+2)) exprActuals)])

-- | 'TLconst' is the AST for a TransLucid constant.
data TLconstData
  = TLconstBool Bool
  | TLconstChar Char
  | TLconstInt Integer
  | TLconstStr String

instance Show TLconstData where
  show (TLconstBool b) = "TLconstBool " ++ show b
  show (TLconstChar c) = "TLconstChar " ++ show c
  show (TLconstInt i) = "TLconstInt " ++ show i
  show (TLconstStr s) = "TLconstStr " ++ show s

-- | 'TLduo' is the AST for a TransLucid binary operator.
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

-- | 'TLuno' is the AST for a TransLucid unary operator.
data TLuno = TLunNot
           | TLunNegate
  deriving Show

-- | 'TLextCtxRange' is for the external context range
type TLextCtxRange = [(String,(Integer,Integer))]

-- | 'TLeval' is the AST for a TransLucid demand.
data TLeval = TLevalExpr TLexpr [(String,(Integer,Integer))]
            | TLevalExprError String
  deriving Show

instance ShowN TLeval where
  showN (TLevalExpr expr ranges) n =
    indentN n ++
    "TLevalExpr " ++ showN expr (n+2) ++
    "\n" ++ indentN (n+2) ++
    show ranges ++ "\n"
  showN (TLevalExprError err) n =
    indentN n ++
    "TLevalExprError " ++ show err ++ "\n"
