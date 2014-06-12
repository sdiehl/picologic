module Picologic.Pretty (
  ppExprU,
  ppExprA,
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
  Conj e1 e2    -> parens $ ppExprU e1 <+> char '∧' <+> ppExprU e2
  Disj e1 e2    -> parens $ ppExprU e1 <+> char '∨' <+> ppExprU e2
  Implies e1 e2 -> parens $ ppExprU e1 <+> char '→' <+> ppExprU e2
  Iff e1 e2     -> parens $ ppExprU e1 <+> char '↔' <+> ppExprU e2

-- | Pretty print with ascii symbols.
ppExprA :: Expr -> Doc
ppExprA ex = case ex of
  Var (Ident n)  ->  text n
  Neg expr       ->  char '~' <> ppExprA expr
  Conj e1 e2     ->  parens $ ppExprA e1 <+> char '&' <+> ppExprA e2
  Disj e1 e2     ->  parens $ ppExprA e1 <+> char '|' <+> ppExprA e2
  Implies e1 e2  ->  parens $ ppExprA e1 <+> text "->" <+> ppExprA e2
  Iff e1 e2      ->  parens $ ppExprA e1 <+> text "<->" <+> ppExprA e2

ppSolutions :: Solutions -> String
ppSolutions (Solutions xs) =
  concat (concat $ intersperse ["\n"] (fmap showExprs xs))

showExprs :: [Expr] -> [String]
showExprs xs = intersperse " " $ fmap (render . ppExprU) xs
