{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Picologic.AST (
  Expr(..),
  Ident(..),
  Solutions(..),
  Ctx,

  variables,
  eval,

  cnf,
  nnf,
  simp,
) where

import Data.List
import Data.Data
import Data.Maybe
import qualified Data.Map as M

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Data, Typeable)

newtype Solutions = Solutions [[Expr]]

type Ctx = M.Map Ident Bool

data Expr
  = Var       Ident      -- ^ Variable
  | Neg       Expr       -- ^ Logical negation
  | Conj      Expr Expr  -- ^ Logical conjunction
  | Disj      Expr Expr  -- ^ Logical disjunction
  | Iff       Expr Expr  -- ^ Logical biconditional
  | Implies   Expr Expr  -- ^ Material implication
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Evaluate expression.
eval :: Ctx -> Expr -> Bool
eval vs (Var v)           = fromMaybe False (M.lookup v vs)
eval vs (Neg expr)        = not $ eval vs expr
eval vs (Conj e1 e2)      = eval vs e1 && eval vs e2
eval vs (Disj   e1 e2)    = eval vs e1 || eval vs e2
eval vs (Implies   e1 e2) = not (eval vs e1) || eval vs e2
eval vs (Iff e1 e2)       = eval vs e1 == eval vs e2

-- | Variables in expression
variables :: Expr -> [Ident]
variables expr = map head . group . sort $ go expr []
  where
    go (Var v)         !vs = v : vs
    go (Neg e)         !vs = go e vs
    go (Conj e1 e2)    !vs = go e1 vs ++ go e2 vs
    go (Disj e1 e2)    !vs = go e1 vs ++ go e2 vs
    go (Iff e1 e2)     !vs = go e1 vs ++ go e2 vs
    go (Implies e1 e2) !vs = go e1 vs ++ go e2 vs

-- | Negation normal form.
nnf :: Expr -> Expr
nnf ex = case ex of
  e@(Var _)             -> e
  e@(Neg (Var _))       -> e
  Neg (Neg e)           -> e

  Conj e1 e2            -> nnf e1 `Conj` nnf e2
  Neg (Conj e1 e2)      -> nnf $ Neg e1 `Disj` Neg e2

  Disj e1 e2            -> nnf e1 `Disj` nnf e2
  Neg (Disj e1 e2)      -> nnf $ Neg e1 `Conj` Neg e2

  Implies e1 e2         -> nnf $ Neg e1 `Disj` e2
  Neg (Implies e1 e2)   -> nnf $ e1 `Conj` Neg e2

  Iff e1 e2             -> let a = e1 `Disj` Neg e2
                               b = Neg e1 `Disj` e2
                               in nnf $ a `Conj` b

  Neg (Iff e1 e2)       -> let a = e1 `Disj` e2
                               b = Neg e1 `Disj` Neg e2
                               in nnf $ a `Conj` b

-- | Conjunctive normal form.
cnf :: Expr -> Expr
cnf = simp . cnf' . nnf
  where
    cnf' :: Expr -> Expr
    cnf' (Conj e1 e2) = cnf' e1 `Conj` cnf' e2
    cnf' (Disj e1 e2) = cnf' e1 `dist` cnf' e2
    cnf' e            = e

    dist :: Expr -> Expr -> Expr
    dist (Conj e11 e12) e2 = (e11 `dist` e2) `Conj` (e12 `dist` e2)
    dist e1 (Conj e21 e22) = (e1 `dist` e21) `Conj` (e1 `dist` e22)
    dist e1 e2             = e1 `Disj` e2

-- | Remove tautologies.
simp :: Expr -> Expr
simp ex = case ex of
  Disj e1 (Neg e2) | e1 == e2 -> e1
  Disj (Neg e1) e2 | e1 == e2 -> e1
  Disj e1 e2 -> Disj (simp e1) (simp e2)
  Conj e1 e2       | e1 == e2 -> e1
                   | otherwise -> Conj (simp e1) (simp e2)
  e -> e
