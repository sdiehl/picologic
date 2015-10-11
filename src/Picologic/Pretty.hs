module Picologic.Pretty (
  ppExprU,
  ppExprA,
  ppExprLisp,
  ppSolutions,
) where

import Picologic.AST (Expr(..), Ident(..), Solutions(..))
import Text.PrettyPrint
import Data.List (intersperse)

-- | Pretty print with unicode symbols.
ppExprU :: Expr -> Doc
ppExprU ex = case ex of
  Var (Ident n) -> text n
  Neg expr      -> char '¬' <> ppExprU expr
  Conj a b      -> con '∧' a b
  Disj a b      -> con '∨' a b
  Implies a b   -> con '→' a b
  Iff a b       -> con '↔' a b
  where con c a b =
          parens $ sep [ppExprU a, char c <+> ppExprU b]

-- | Pretty print with ascii symbols.
ppExprA :: Expr -> Doc
ppExprA ex = case ex of
  Var (Ident n)  ->  text n
  Neg expr       ->  char '~' <> ppExprA expr
  Conj e1 e2     ->  parens $ ppExprA e1 <+> char '&' <+> ppExprA e2
  Disj e1 e2     ->  parens $ ppExprA e1 <+> char '|' <+> ppExprA e2
  Implies e1 e2  ->  parens $ ppExprA e1 <+> text "->" <+> ppExprA e2
  Iff e1 e2      ->  parens $ ppExprA e1 <+> text "<->" <+> ppExprA e2

-- | Pretty print into S-Expressions
ppExprLisp :: Expr -> Doc
ppExprLisp ex = case ex of
  Var (Ident n)       -> text n
  Conj a b            -> con "and" $ ands [a, b]
  Disj a b            -> con "or" $ ors [a, b]
  Implies a b         -> con "==>" [a, b]
  Iff a b             -> con "==" $ iffs [a, b]
  Neg (Var (Ident n)) -> text $ "-" ++ n
  Neg (Conj a b)      -> con "nand" $ ands [a, b]
  Neg (Disj a b)      -> con "nor" $ ors [a, b]
  Neg (Iff a b)       -> con "xor" $ iffs [a, b]
  Neg expr            -> parens $ text "not" <+> ppExprLisp expr
  where con c xs =
          parens $
          sep [text c,
               nest 1 $ sep $ map ppExprLisp xs]

ands [] = []
ands (Conj a b : xs) = ands [a] ++ ands [b] ++ ands xs
ands (x:xs) = x : ands xs

ors [] = []
ors (Disj a b : xs) = ors [a] ++ ors [b] ++ ors xs
ors (x:xs) = x : ors xs

iffs [] = []
iffs (Iff a b : xs) = iffs [a] ++ iffs [b] ++ iffs xs
iffs (x:xs) = x : iffs xs

instance Show Expr where
  show = show . ppExprLisp


ppSolutions :: Solutions -> String
ppSolutions (Solutions xs) =
  concat (concat $ intersperse ["\n"] (fmap showExprs xs))

showExprs :: [Expr] -> [String]
showExprs xs = intersperse " " $ fmap (render . ppExprU) xs
