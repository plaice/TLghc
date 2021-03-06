{- |
Module      : TL.Parse
Description : The module for parsing TransLucid programs
Copyright   : (c) John Plaice, 2020
License     : GPL-3
Maintainer  : johnplaice@gmail.com
Stability   : experimental
Portability : Portable
-}
module TL.Parse (parseText)
where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token
import qualified Data.Array as Array
import qualified Data.Map as Map
import TL.AST
import System.IO

parseText :: String -> Either ParseError TLfile
parseText = parse parserFile ""

parserFile :: Parser TLfile
parserFile =
  do
    fileContents <- parserFileContents
    eof
    return fileContents

parserFileContents :: Parser TLfile
parserFileContents =
  do
    decls <- many parserDecl
    let dims = filter isDeclDim decls
    let vars = filter isDeclVar decls
    let funcs = filter isDeclFunc decls
    let dimErrors = filter isDeclDimError decls
    let valErrors = filter isDeclVarError decls
    let funErrors = filter isDeclFunError decls
    evals <- many parserEvalExpr
    let evalExprs = filter isEvalExpr evals
    let evalErrors = filter isEvalExprError evals
    return (TLfile dims vars funcs evalExprs
            dimErrors valErrors funErrors evalErrors)

parserDecl :: Parser TLdecl
parserDecl =
        parserDeclDim
    <|> parserDeclVar

parserDeclDim :: Parser TLdecl
parserDeclDim =
  try (do
         m_reserved "dim"
         d <- m_identifier
         m_reservedOp "<-"
         TLdeclDim d <$> parserExpr
      )
  <|> do
        m_reserved "dim"
        chars <- manyTill anyChar (lookAhead parserAnyKeyWord)
        return (TLdeclDimError ("dim " ++ chars))

parserDeclVar :: Parser TLdecl
parserDeclVar =
      parserDeclVarSimple
  <|> parserDeclVarDim
  <|> parserDeclVarInt
  <|> parserDeclVarDimInt
  <|> do
        m_reserved "var"
        chars <- manyTill anyChar (lookAhead parserAnyKeyWord)
        return (TLdeclVarError ("var " ++ chars))
  <|> do
        m_reserved "fun"
        chars <- manyTill anyChar (lookAhead parserAnyKeyWord)
        return (TLdeclFunError ("fun " ++ chars))

parserDeclVarSimple :: Parser TLdecl
parserDeclVarSimple =
  try (do
         m_reserved "var"
         x <- m_identifier
         m_reservedOp "="
         TLdeclVar x <$> parserExpr
      )

parserDeclVarDim :: Parser TLdecl
parserDeclVarDim =
  try (do
         m_reserved "fun"
         x <- m_identifier
         m_reservedOp "."
         dimArgs <- parserIdsDots
         m_reservedOp "="
         TLdeclFunc x dimArgs [] <$> parserExpr
      )

parserDeclVarInt :: Parser TLdecl
parserDeclVarInt =
  try (do
         m_reserved "fun"
         x <- m_identifier
         varArgs <- m_parens parserIds
         m_reservedOp "="
         TLdeclFunc x [] varArgs <$> parserExpr
      )

parserDeclVarDimInt :: Parser TLdecl
parserDeclVarDimInt =
  try (do
         m_reserved "fun"
         x <- m_identifier
         m_reservedOp "."
         dimArgs <- parserIdsDots
         varArgs <- m_parens parserIds
         m_reservedOp "="
         TLdeclFunc x dimArgs varArgs <$> parserExpr
      )

parserAnyKeyWord :: Parser ()
parserAnyKeyWord =
        m_reserved "dim"
    <|> m_reserved "var"
    <|> m_reserved "fun"
    <|> m_reserved "end"
    <|> m_reserved "where"
    <|> m_reserved "evalExpr"
    <|> eof

parserEvalExpr :: Parser TLeval
parserEvalExpr =
  try (do
         m_reserved "evalExpr"
         expr <- parserExpr
         m_reservedOp "@"
         pairs <- m_brackets parserDataCtx
         return (TLevalExpr expr pairs)
      )
  <|> do
        m_reserved "evalExpr"
        chars <- manyTill anyChar (lookAhead parserAnyKeyWord)
        return (TLevalExprError ("evalExpr " ++ chars))

parserCtx :: Parser [(String, TLexpr)]
parserCtx = m_commaSep1 parserCtxPair

parserCtxPair :: Parser (String, TLexpr)
parserCtxPair =
  do
    d <- m_identifier
    m_reservedOp "<-"
    expr <- parserExpr
    return (d, expr)

parserDataCtx :: Parser [(String, (Integer,Integer))]
parserDataCtx = m_commaSep parserDataCtxPair

parserDataCtxPair :: Parser (String, (Integer,Integer))
parserDataCtxPair =
  do
    d <- m_identifier
    m_reservedOp "<-"
    datumLow <- m_integer
    m_reservedOp ".."
    datumHigh <- m_integer
    return (d, (datumLow, datumHigh))

parserExpr :: Parser TLexpr
parserExpr = parserExprWhereMaybe

parserExprWhereMaybe :: Parser TLexpr
parserExprWhereMaybe = parserExprWhereNo
                   <|> parserExprWhereYes

parserExprWhereNo :: Parser TLexpr
parserExprWhereNo =
  try (do
         expr <- parserExprCondMaybe
         notFollowedBy (m_reserved "where")
         return expr
      )

parserExprWhereYes :: Parser TLexpr
parserExprWhereYes =
  try (do
         arg <- parserExprCondMaybe
         m_reserved "where"
         decls <- many parserDecl
         let dims = filter isDeclDim decls
         let vars = filter isDeclVar decls
         let funcs = filter isDeclFunc decls
         m_reserved "end"
         return (TLwhere dims vars funcs arg)
      )

parserExprCondMaybe :: Parser TLexpr
parserExprCondMaybe = parserExprCondNo
                   <|> parserExprCondYes

parserExprCondNo :: Parser TLexpr
parserExprCondNo =
  try (do
         notFollowedBy (m_reserved "if")
         parserExprOps
      )

parserExprCondYes :: Parser TLexpr
parserExprCondYes =
  try (do
         m_reserved "if"
         arg1 <- parserExpr
         m_reserved "then"
         arg2 <- parserExpr
         m_reserved "else"
         TLcond arg1 arg2 <$> parserExprCondMaybe
      )

parserExprOps :: Parser TLexpr
parserExprOps = buildExpressionParser table term <?> "expression"
term = parserExprApplyMaybe
table = [ [ parserUnop  "!"  TLunNot
          , parserUnop  "-"  TLunNegate
          ]
        , [ parserBinop "*"  TLbinTimes AssocLeft
          , parserBinop "/"  TLbinDiv AssocLeft
          , parserBinop "%"  TLbinMod AssocLeft
          , parserBinop "%%" TLbinRem AssocLeft
          ]
        , [ parserBinop "+"  TLbinPlus AssocLeft
          , parserBinop "-"  TLbinMinus AssocLeft
          ]
        , [ parserBinop "<"  TLbinLT AssocNone
          , parserBinop "<=" TLbinLE AssocNone
          , parserBinop ">=" TLbinGE AssocNone
          , parserBinop ">"  TLbinGT AssocNone
          , parserBinop "==" TLbinEQ AssocNone
          , parserBinop "/=" TLbinNE AssocNone
          ]
        , [ parserBinop "&&" TLbinAnd AssocLeft ]
        , [ parserBinop "||" TLbinOr AssocLeft ]
        , [ parserTLop "alt"
          , parserTLop "asa"
          , parserTLop "cby"
          , parserTLop "cby2"
          , parserTLop "cby3"
          , parserTLop "fby"
          , parserTLop "upon"
          , parserTLop "wvr"
          , parserTLop "ybf"
          ]
        ]

parserUnop
  :: String
     -> TLuno -> Operator String u Data.Functor.Identity.Identity TLexpr
parserUnop opToken opTL =
  Prefix (m_reservedOp opToken >> return (TLunop opTL))

parserBinop
  :: String
     -> TLduo
     -> Assoc
     -> Operator String u Data.Functor.Identity.Identity TLexpr
parserBinop opToken opTL =
  Infix (m_reservedOp opToken >> return (TLbinop opTL))

parserTLop token =
  Infix (parserTLopToken token) AssocLeft

parserTLop
  :: String
     -> Operator String u Data.Functor.Identity.Identity TLexpr
parserTLopToken token =
  do
    m_reserved token
    m_reservedOp "."
    d <- m_identifier
    return (\x y -> TLapply (TLvar token) [d] [x,y])

parserExprApplyMaybe :: Parser TLexpr
parserExprApplyMaybe = parserExprApplyNo
                   <|> parserExprApplyDim
                   <|> parserExprApplyInt
                   <|> parserExprApplyDimInt

parserExprApplyNo :: Parser TLexpr
parserExprApplyNo =
  try (do
         expr <- parserExprPerturbMaybe
         notFollowedBy (m_reservedOp ".")
         notFollowedBy (m_reservedOp "(")
         return expr
      )

parserExprApplyDim :: Parser TLexpr
parserExprApplyDim =
  try (do
         func <- parserExprPerturbMaybe
         m_reservedOp "."
         dimActuals <- parserIdsDots
         notFollowedBy (m_reservedOp "(")
         return (TLapply func dimActuals [])
      )

parserExprApplyInt :: Parser TLexpr
parserExprApplyInt =
  try (do
         func <- parserExprPerturbMaybe
         notFollowedBy (m_reservedOp ".")
         exprActuals <- m_parens parserExprs
         return (TLapply func [] exprActuals)
      )

parserExprApplyDimInt :: Parser TLexpr
parserExprApplyDimInt =
  try (do
         func <- parserExprPerturbMaybe
         m_reservedOp "."
         dimActuals <- parserIdsDots
         exprActuals <- m_parens parserExprs
         return (TLapply func dimActuals exprActuals)
      )

parserExprPerturbMaybe :: Parser TLexpr
parserExprPerturbMaybe = parserExprPerturbNo
                     <|> parserExprPerturbYes

parserExprPerturbNo :: Parser TLexpr
parserExprPerturbNo =
  try (do
         arg <- parserExprAtomic
         notFollowedBy (m_reservedOp "[")
         return arg
      )

parserExprPerturbYes :: Parser TLexpr
parserExprPerturbYes =
  try (do
         expr <- parserExprAtomic
         pairs <- m_brackets parserCtx
         return (TLat pairs expr)
      )

parserExprAtomic :: Parser TLexpr
parserExprAtomic = parserExprParens
               <|> parserFnDim
               <|> parserFnInt
               <|> parserFnDimInt
               <|> parserExprId
               <|> parserExprHash
               <|> parserExprData
               <|> parserExprArray

parserFnDim :: Parser TLexpr
parserFnDim =
  try (do
         m_reservedOp "("
         m_reserved "fn"
         m_reservedOp "."
         dimArgs <- parserIdsDots
         m_reservedOp "->"
         arg <- parserExpr
         m_reservedOp ")"
         return (TLfn dimArgs [] arg)
      )

parserFnInt :: Parser TLexpr
parserFnInt =
  try (do
         m_reservedOp "("
         m_reserved "fn"
         varArgs <- m_parens parserIds
         m_reservedOp "->"
         arg <- parserExpr
         m_reservedOp ")"
         return (TLfn [] varArgs arg)
      )

parserFnDimInt :: Parser TLexpr
parserFnDimInt =
  try (do
         m_reservedOp "("
         m_reserved "fn"
         m_reservedOp "."
         dimArgs <- parserIdsDots
         varArgs <- m_parens parserIds
         m_reservedOp "->"
         arg <- parserExpr
         m_reservedOp ")"
         return (TLfn dimArgs varArgs arg)
      )

parserExprParens :: Parser TLexpr
parserExprParens =
  try (do
         m_reservedOp "("
         notFollowedBy (m_reserved "fn")
         expr <- parserExpr
         m_reservedOp ")"
         return expr
      )

parserExprId :: Parser TLexpr
parserExprId =
  try (TLvar <$> m_identifier)

parserExprHash :: Parser TLexpr
parserExprHash =
  try (do
         m_reservedOp "#"
         TLhash <$> m_identifier
      )

parserExprData :: Parser TLexpr
parserExprData =
  try (TLconst <$> parserData)

parserIds :: Parser [String]
parserIds = m_commaSep m_identifier

parserIdsDots :: Parser [String]
parserIdsDots = sepBy1 m_identifier m_dot

parserExprs :: Parser [TLexpr]
parserExprs = m_commaSep parserExpr

parserExprArray :: Parser TLexpr
parserExprArray =
  try (m_braces (do
                   ranges <- m_brackets parserRanges
                   m_comma
                   datas <- m_brackets parserDatas
                   return $ processArray ranges datas
                ))

parserRanges :: Parser [(String,Integer,Integer)]
parserRanges = m_commaSep (m_parens parserRange)

parserRange :: Parser (String,Integer,Integer)
parserRange =
  try (do
         dim <- m_identifier
         m_comma
         min <- m_integer
         m_comma
         max <- m_integer
         return (dim,min,max)
      )

parserDatas :: Parser [TLconstData]
parserDatas = m_commaSep parserData

parserData :: Parser TLconstData
parserData =
        parserBool "true" True
    <|> parserBool "false" False
    <|> TLconstChar <$> m_charLiteral
    <|> TLconstInt <$> m_integer
    <|> TLconstStr <$> m_stringLiteral

parserBool opToken value =
  m_reserved opToken >> return (TLconstBool value)

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter <|> char '_'
              , identLetter = alphaNum
              , opStart = opLetter emptyDef
              , opLetter = oneOf "!*/%+=<>=/."
              , reservedOpNames = [ "+", "-"
                                  , "*", "%", "%%", "/"
                                  , "<", ">", "<=", ">=", "==", "/="
                                  , "&&", "||"
                                  , "<-", ".", "->"
                                  ]
              , reservedNames = ["if", "then", "else",
                                 "true", "false",
                                 "where", "var", "fun", "dim", "end",
                                 "fn"]
              , caseSensitive = True
              }

TokenParser{ parens = m_parens
           , braces = m_braces
           , brackets = m_brackets
           , charLiteral = m_charLiteral
           , comma = m_comma
           , commaSep = m_commaSep
           , commaSep1 = m_commaSep1
           , dot = m_dot
           , identifier = m_identifier
           , integer = m_integer
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , stringLiteral = m_stringLiteral
           , whiteSpace = m_whiteSpace
           } = makeTokenParser def

processArray
  :: [(String, Integer, Integer)] -> [TLconstData] -> TLexpr

processArray [(dim1, min1, max1)] datas
  | (toInteger . length $ datas) /=
    size1 =
    error "Array1 not the correct length"
  | otherwise = TLarray1 dim1
                         (Array.array
                          (min1, max1)
                          [(i, datas !! fromInteger (i-min1)) |
                           i <- [min1 .. max1]])
  where size1 = max1 - min1 + 1

processArray [(dim1, min1, max1),
              (dim2, min2, max2)] datas
  | (toInteger . length $ datas) /=
    (size1 * size2) =
    error "Array2 not the correct length"
  | otherwise = TLarray2 (dim1, dim2)
                         (Array.array
                          ((min1, min2),
                           (max1, max2))
                          [((i1,i2), datas !! fromInteger d) |
                           i1 <- [min1 .. max1],
                           i2 <- [min2 .. max2],
                           let d = (i2-min2) + size2 *
                                   (i1-min1)
                          ])
  where size1 = max1 - min1 + 1
        size2 = max2 - min2 + 1

processArray [(dim1, min1, max1),
              (dim2, min2, max2),
              (dim3, min3, max3)] datas
  | (toInteger . length $ datas) /=
    (size1 * size2 * size3) =
    error "Array3 not the correct length"
  | otherwise = TLarray3 (dim1, dim2, dim3)
                         (Array.array
                          ((min1, min2, min3),
                           (max1, max2, max3))
                          [((i1,i2,i3), datas !! fromInteger d) |
                           i1 <- [min1 .. max1],
                           i2 <- [min2 .. max2],
                           i3 <- [min3 .. max3],
                           let d = (i3-min3) + size3 *
                                   ((i2-min2) + size2 *
                                    (i1-min1))
                          ])
  where size1 = max1 - min1 + 1
        size2 = max2 - min2 + 1
        size3 = max3 - min3 + 1

processArray [(dim1, min1, max1),
              (dim2, min2, max2),
              (dim3, min3, max3),
              (dim4, min4, max4)] datas
  | (toInteger . length $ datas) /=
    (size1 * size2 * size3 * size4) =
    error "Array4 not the correct length"
  | otherwise = TLarray4 (dim1, dim2, dim3, dim4)
                         (Array.array
                          ((min1, min2, min3, min4),
                           (max1, max2, max3, max4))
                          [((i1,i2,i3,i4), datas !! fromInteger d) |
                           i1 <- [min1 .. max1],
                           i2 <- [min2 .. max2],
                           i3 <- [min3 .. max3],
                           i4 <- [min4 .. max4],
                           let d = (i4-min4) + size4 *
                                   ((i3-min3) + size3 *
                                    ((i2-min2) + size2 *
                                     (i1-min1)))
                          ])
  where size1 = max1 - min1 + 1
        size2 = max2 - min2 + 1
        size3 = max3 - min3 + 1
        size4 = max4 - min4 + 1

processArray [(dim1, min1, max1),
              (dim2, min2, max2),
              (dim3, min3, max3),
              (dim4, min4, max4),
              (dim5, min5, max5)] datas
  | (toInteger . length $ datas) /=
    (size1 * size2 * size3 * size4 * size5) =
    error "Array5 not the correct length"
  | otherwise = TLarray5 (dim1, dim2, dim3, dim4, dim5)
                         (Array.array
                          ((min1, min2, min3, min4, min5),
                           (max1, max2, max3, max4, max5))
                          [((i1,i2,i3,i4,i5), datas !! fromInteger d) |
                           i1 <- [min1 .. max1],
                           i2 <- [min2 .. max2],
                           i3 <- [min3 .. max3],
                           i4 <- [min4 .. max4],
                           i5 <- [min5 .. max5],
                           let d = (i5-min5) + size5 *
                                   ((i4-min4) + size4 *
                                    ((i3-min3) + size3 *
                                     ((i2-min2) + size2 *
                                      (i1-min1))))
                          ])
  where size1 = max1 - min1 + 1
        size2 = max2 - min2 + 1
        size3 = max3 - min3 + 1
        size4 = max4 - min4 + 1
        size5 = max5 - min5 + 1

isDeclDim :: TLdecl -> Bool
isDeclDim (TLdeclDim _ _) = True
isDeclDim _               = False

isDeclVar :: TLdecl -> Bool
isDeclVar (TLdeclVar _ _) = True
isDeclVar _               = False

isDeclFunc :: TLdecl -> Bool
isDeclFunc (TLdeclFunc _ _ _ _) = True
isDeclFunc _                    = False

isDeclDimError :: TLdecl -> Bool
isDeclDimError (TLdeclDimError _) = True
isDeclDimError _                  = False

isDeclVarError :: TLdecl -> Bool
isDeclVarError (TLdeclVarError _) = True
isDeclVarError _                  = False

isDeclFunError :: TLdecl -> Bool
isDeclFunError (TLdeclFunError _) = True
isDeclFunError _                  = False

isEvalExpr :: TLeval -> Bool
isEvalExpr (TLevalExpr _ _) = True
isEvalExpr _                = False

isEvalExprError :: TLeval -> Bool
isEvalExprError (TLevalExprError _) = True
isEvalExprError _                   = False
