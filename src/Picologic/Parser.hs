module Picologic.Parser (
  parseFile,
  parseExpr,
  readExpr,
) where

import Text.Parsec hiding (State)
import qualified Text.Parsec.Expr as Ex

import Picologic.AST
import Picologic.Lexer

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

prefixOp :: String -> (a -> a) -> Op a
prefixOp x f = Ex.Prefix (reservedOp x >> return f)

operators :: [[Op Expr]]
operators = [
    [ prefixOp "~" Neg ],
    [
      infixOp "&" Conj Ex.AssocLeft,
      infixOp "|" Disj Ex.AssocLeft,
      infixOp "->" Implies Ex.AssocLeft,
      infixOp "<->" Iff Ex.AssocLeft
    ]
  ]

var :: Parser Expr
var = do
  x <- identifier
  return $ Var (Ident x)

cexpr :: Parser Expr
cexpr =  Ex.buildExpressionParser operators cfactor

cfactor :: Parser Expr
cfactor =  var
       <|> parens cexpr

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents cexpr) "<stdin>"

readExpr :: String -> Expr
readExpr s = case parseExpr s of
  Left err -> error (show err)
  Right expr -> expr

parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile fname = do
  fcontents <- readFile fname
  return $ parse (contents cexpr) fname fcontents
