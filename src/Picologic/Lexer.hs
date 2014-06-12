module Picologic.Lexer (
  Parser,
  Op,
  contents,
  parens,
  reservedOp,
  reserved,
  identifier,
) where

import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

type Parser   = ParsecT String () Identity
type Lexer    = Tok.GenTokenParser String () Identity
type Language = Tok.GenLanguageDef String () Identity
type Op       = Ex.Operator String () Identity

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

reservedOps :: [String]
reservedOps = [
    "->",
    "&",
    "|",
    "<->",
    "~"
  ]

reservedNames :: [String]
reservedNames = []

lexerStyle :: Language
lexerStyle = haskellStyle
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum
  , Tok.opStart         = Tok.opLetter lexerStyle
  , Tok.opLetter        = oneOf "`~!@$%^&*-+=;:<>./?#"
  , Tok.reservedOpNames = reservedOps
  , Tok.reservedNames   = reservedNames
  , Tok.caseSensitive   = True
  }

lexer :: Lexer
lexer = Tok.makeTokenParser lexerStyle

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
