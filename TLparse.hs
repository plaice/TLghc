module TLparse (playFile,parserFile,playExpr,parserExpr)
where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Data.Array as Array
import qualified Data.Map as Map
import TL

playFile :: String -> IO ()
playFile inp = case parse parserFile "" inp of
                 Left err -> print err
                 Right ans -> print (evalFile ans)

parserFile :: Parser TLfile
parserFile =
  do
    decls <- many parserDecl
    let dims = filter isDeclDim decls
    let vars = filter isDeclVar decls
    let funcs = filter isDeclFunc decls
    evalExprs <- many parserEvalExpr
    return (TLfile dims vars funcs evalExprs)

playExpr :: String -> IO ()
playExpr inp = case parse parserExpr "" inp of
                 Left err -> print err
                 Right ans -> print ans

parserExpr :: Parser TLexpr
parserExpr = parserExprSimple
         <|> parserExprApply
         <|> parserExprWhere

parserDecl :: Parser TLdecl
parserDecl =
        do
          decl <- parserDeclDim
          return decl
    <|> do
          decl <- parserDeclVar
          return decl
    <|> do
          decl <- parserDeclFunc
          return decl

parserDeclDim :: Parser TLdecl
parserDeclDim =
  do
    m_reserved "dim"
    d <- m_identifier
    m_reservedOp "<-"
    expr <- parserExpr
    return (TLdeclDim d expr)

parserDeclVar :: Parser TLdecl
parserDeclVar =
  do
    m_reserved "var"
    x <- m_identifier
    m_reservedOp "="
    expr <- parserExpr
    return (TLdeclVar x expr)

parserDeclFunc :: Parser TLdecl
parserDeclFunc =
  do
    m_reserved "fun"
    f <- m_identifier
    m_reservedOp "."
    dimArgs <- parserIds
    varArgs <- m_parens parserIds
    m_reservedOp "="
    arg <- parserExpr
    return (TLdeclFunc f dimArgs varArgs arg)

parserEvalExpr :: Parser TLeval
parserEvalExpr =
  do
    m_reserved "evalExpr"
    expr <- parserExpr
    m_reservedOp "@"
    pairs <- m_brackets parserDataCtx
    return (TLevalExpr expr pairs)

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

parserExprSimple :: Parser TLexpr
parserExprSimple =
  try (do
         arg <- parserExprOps
         notFollowedBy (m_reservedOp ".")
         notFollowedBy (m_reserved "where")
         return arg
      )

parserExprApply :: Parser TLexpr
parserExprApply =
  try (do
         func <- parserExprOps
         notFollowedBy (m_reserved "where")
         m_reservedOp "."
         dimActuals <- parserIdsDots
         exprActuals <- m_parens parserExprs
         return (TLapply func dimActuals exprActuals)
      )

parserExprWhere :: Parser TLexpr
parserExprWhere =
  try (do
         arg <- parserExprOps
         m_reserved "where"
         decls <- many parserDecl
         let dims = filter isDeclDim decls
         let vars = filter isDeclVar decls
         let funcs = filter isDeclFunc decls
         m_reserved "end"
         return (TLwhere dims vars funcs arg)
      )

parserIds :: Parser [String]
parserIds = m_commaSep m_identifier

parserIdsDots :: Parser [String]
parserIdsDots = sepBy1 m_identifier m_dot

parserExprs :: Parser [TLexpr]
parserExprs = m_commaSep parserExpr

parserExprOps :: Parser TLexpr
parserExprOps = buildExpressionParser table term <?> "expression"
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
          , parserBinop "!=" TLbinNE AssocNone
          ]
        , [ parserBinop "&&" TLbinAnd AssocLeft ]
        , [ parserBinop "||" TLbinOr AssocLeft ]
        , [ parserTLop "fby"
          , parserTLop "ybf"
          , parserTLop "asa"
          , parserTLop "wvr"
          , parserTLop "upon"
          ]
        ]

term =     parserExprParens
       <|> parserExprParensPerturb
       <|> parserExprId
       <|> parserExprIdPerturb
       <|> m_brackets parserExprArray
       <|> do
             datum <- parserData
             return (TLconst datum)
       <|> do
             m_reservedOp "#"
             d <- m_identifier
             return (TLhash d)
       <|> do
             m_reserved "if"
             arg1 <- parserExpr
             m_reserved "then"
             arg2 <- parserExpr
             m_reserved "else"
             arg3 <- parserExpr
             return (TLcond arg1 arg2 arg3)
       <|> do
             m_reserved "fn"
             m_reservedOp "."
             dimArgs <- parserIdsDots
             varArgs <- m_parens parserIds
             m_reservedOp "->"
             arg <- parserExpr
             return (TLfn dimArgs varArgs arg)

parserTLopToken token =
  do
    m_reserved token
    m_reservedOp "."
    d <- m_identifier
    return (\x y -> TLapply (TLvar token) [d] [x,y])

parserTLop token =
  Infix (parserTLopToken token) AssocLeft

parserUnop opToken opTL =
  Prefix (m_reservedOp opToken >> return (\x -> TLunop opTL x))

parserBinop opToken opTL assoc =
  Infix (m_reservedOp opToken >> return (\x y -> TLbinop opTL x y)) assoc

parserExprParens :: Parser TLexpr
parserExprParens =
  try (do
         arg <- m_parens parserExpr
         notFollowedBy (char '[')
         return arg
      )

parserExprParensPerturb :: Parser TLexpr
parserExprParensPerturb =
  try (do
         arg <- m_parens parserExpr
         pairs <- m_brackets parserCtx
         return (TLat pairs arg)
      )

parserExprId :: Parser TLexpr
parserExprId =
  try (do
         x <- m_identifier
         notFollowedBy (char '[')
         return (TLvar x)
      )

parserExprIdPerturb :: Parser TLexpr
parserExprIdPerturb =
  try (do
         x <- m_identifier
         pairs <- m_brackets parserCtx
         return (TLat pairs (TLvar x))
      )

parserExprArray :: Parser TLexpr
parserExprArray =
  do
    ranges <- m_brackets parserRanges
    m_comma
    datas <- m_brackets parserDatas
    return $ processArray ranges datas

parserRanges :: Parser [(String,Integer,Integer)]
parserRanges = m_commaSep (m_parens parserRange)

parserRange :: Parser (String,Integer,Integer)
parserRange =
  do
    dim <- m_identifier
    m_comma
    min <- m_integer
    m_comma
    max <- m_integer
    return (dim,min,max)

parserDatas :: Parser [TLdata]
parserDatas = m_commaSep parserData

parserData :: Parser TLdata
parserData =
        (parserBool "true" True)
    <|> (parserBool "false" False)
    <|> do
          c <- m_charLiteral
          return (TLchar c)
    <|> do
          i <- m_integer
          return (TLint i)
    <|> do
          s <- m_stringLiteral
          return (TLstr s)

parserBool opToken value =
  m_reserved opToken >> return (TLbool value)

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter <|> char '_'
              , identLetter = alphaNum
              , opStart = opLetter emptyDef
              , opLetter = oneOf "!*/%+=<>=/#@."
              , reservedOpNames = [ "+", "-"
                                  , "*", "%", "%%", "/"
                                  , "<", ">", "<=", ">=", "==", "/="
                                  , "&&", "||"
                                  , "#", "@"
                                  , "<-", ".", "->"
                                  ]
              , reservedNames = ["negate", "div", "mod", "rem",
                                 "not", "and", "or",
                                 "if", "then", "else",
                                 "true", "false",
                                 "where", "var", "fun", "dim", "end",
                                 "fn", "let", "in"]
              , caseSensitive = True
              }

TokenParser{ parens = m_parens
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

processArray [(dim1, min1, max1)] datas
  | (toInteger . length $ datas) /=
    size1 =
    error "Array1 not the correct length"
  | otherwise = TLarray1 dim1
                         (Array.array
                          (min1, max1)
                          [(i, datas !! (fromInteger (i-min1))) |
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
                          [((i1,i2), datas !! (fromInteger d)) |
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
                          [((i1,i2,i3), datas !! (fromInteger d)) |
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
                          [((i1,i2,i3,i4), datas !! (fromInteger d)) |
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
                          [((i1,i2,i3,i4,i5), datas !! (fromInteger d)) |
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
