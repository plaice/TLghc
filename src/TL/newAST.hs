{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}

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
{-
   TLfile(..)
  ,TLdecl(..)
-}
   TLexpr(..)
{-
  ,TLconstData(..)
  ,TLduo(..)
  ,TLuno(..)
  ,TLeval(..)
  ,TLextCtxRange(..)
-}
)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List
import Data.Maybe(fromMaybe)

{-
-- | 'TLfile' is the AST for a TransLucid program.
data TLfile
  = TLfile [TLdecl] [TLdecl] [TLdecl] [TLeval]
           [TLdecl] [TLdecl] [TLdecl] [TLeval]

-- | 'TLdecl' is the AST for a TransLucid declaration.
data TLdecl
  = TLdeclDim String TLexpr                     -- ^ dimension identifier
  | TLdeclDimError String                       -- ^ syntax error in dim decl
  | TLdeclVar String TLexpr                     -- ^ variable identifier
  | TLdeclVarError String                       -- ^ syntax error in var decl
  | TLdeclFunc String [String] [String] TLexpr  -- ^ function identifier
  | TLdeclFunError String                       -- ^ syntax error in fun decl
    deriving Show
-}

-- | 'TLexpr' is the AST for a TransLucid expression.
data TLexpr a where
    TLconstBool :: Bool -> TLexpr Bool
    TLconstChar :: Char -> TLexpr Char
    TLconstInt  :: Integer -> TLexpr Integer
    TLconstStr  :: String -> TLexpr String
    TLvar       :: String -> TLexpr a
    TLunNot     :: TLexpr Bool -> TLexpr Bool
    TLunNegate  :: TLexpr Integer -> TLexpr Integer
    TLbinAnd    :: TLexpr Bool -> TLexpr Bool -> TLexpr Bool
    TLbinOr     :: TLexpr Bool -> TLexpr Bool -> TLexpr Bool
    TLbinPlus   :: TLexpr Integer -> TLexpr Integer -> TLexpr Integer
    TLbinMinus  :: TLexpr Integer -> TLexpr Integer -> TLexpr Integer
    TLbinTimes  :: TLexpr Integer -> TLexpr Integer -> TLexpr Integer
    TLbinDiv    :: TLexpr Integer -> TLexpr Integer -> TLexpr Integer
    TLbinMod    :: TLexpr Integer -> TLexpr Integer -> TLexpr Integer
    TLbinRem    :: TLexpr Integer -> TLexpr Integer -> TLexpr Integer
    TLbinLT     :: TLexpr Integer -> TLexpr Integer -> TLexpr Bool
    TLbinLE     :: TLexpr Integer -> TLexpr Integer -> TLexpr Bool
    TLbinGE     :: TLexpr Integer -> TLexpr Integer -> TLexpr Bool
    TLbinGT     :: TLexpr Integer -> TLexpr Integer -> TLexpr Bool
    TLbinEQ     :: Eq a => TLexpr a -> TLexpr a -> TLexpr Bool
    TLbinNE     :: Eq a => TLexpr a -> TLexpr a -> TLexpr Bool
    TLcond      :: TLexpr Bool -> TLexpr a -> TLexpr a -> TLexpr a
    TLhash      :: String -> TLexpr Integer
    TLat        :: TLexpr a -> [(String, TLexpr Integer)] -> TLexpr a

-- | 'TLlistCtx' is for the evaluation context.
type TLlistCtx = Map.Map String Integer

eval :: TLexpr a -> TLlistCtx -> a
eval (TLconstBool b)    ctx = b
eval (TLconstChar c)    ctx = c
eval (TLconstInt i)     ctx = i
eval (TLconstStr s)     ctx = s
eval (TLunNot e)        ctx = not (eval e ctx)
eval (TLunNegate e)     ctx = negate (eval e ctx)
eval (TLbinAnd e1 e2)   ctx = eval e1 ctx && eval e2 ctx
eval (TLbinOr e1 e2)    ctx = eval e1 ctx || eval e2 ctx
eval (TLbinPlus e1 e2)  ctx = eval e1 ctx + eval e2 ctx
eval (TLbinMinus e1 e2) ctx = eval e1 ctx - eval e2 ctx
eval (TLbinTimes e1 e2) ctx = eval e1 ctx * eval e2 ctx
eval (TLbinDiv e1 e2)   ctx = eval e1 ctx `div` eval e2 ctx
eval (TLbinMod e1 e2)   ctx = eval e1 ctx `mod` eval e2 ctx
eval (TLbinRem e1 e2)   ctx = eval e1 ctx `rem` eval e2 ctx
eval (TLbinLT e1 e2)    ctx = eval e1 ctx < eval e2 ctx
eval (TLbinLE e1 e2)    ctx = eval e1 ctx <= eval e2 ctx
eval (TLbinGE e1 e2)    ctx = eval e1 ctx >= eval e2 ctx
eval (TLbinGT e1 e2)    ctx = eval e1 ctx > eval e2 ctx
eval (TLbinEQ e1 e2)    ctx = eval e1 ctx == eval e2 ctx
eval (TLbinNE e1 e2)    ctx = eval e1 ctx /= eval e2 ctx
eval (TLcond e1 e2 e3)  ctx = case eval e1 ctx of
                                True -> eval e2 ctx
                                False -> eval e3 ctx
eval (TLhash d)         ctx =
  fromMaybe (error $ "eval: Did not find dim " ++ d) ord
  where ord = Map.lookup d ctx
eval (TLat expr pairs)  ctx =
  eval expr (ctxPerturb (map (\(d,e) -> (d, eval e ctx)) pairs) ctx)
  where ctxPerturb newPairs ctx = Map.union (Map.fromList newPairs) ctx

{-
  | TLvar String
  | TLwhere [TLdecl] [TLdecl] [TLdecl] TLexpr
  | TLfn [String] [String] TLexpr
  | TLapply TLexpr [String] [TLexpr]
  deriving Show
-}
{-
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
-}

{-
-- | 'TLextCtxRange' is for the external context range
type TLextCtxRange = [(String,(Integer,Integer))]


-- | 'TLeval' is the AST for a TransLucid demand.
data TLeval = TLevalExpr TLexpr [(String,(Integer,Integer))]
            | TLevalExprError String
  deriving Show
-}
