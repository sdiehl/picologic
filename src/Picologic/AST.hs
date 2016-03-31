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
  isConst,
  propConst
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
  | Top                  -- ^ Constant true
  | Bottom               -- ^ Constant false
  deriving (Eq, Ord, Data, Typeable)

-- | Evaluate expression.
eval :: Ctx -> Expr -> Bool
eval vs (Var v)           = fromMaybe False (M.lookup v vs)
eval vs (Neg expr)        = not $ eval vs expr
eval vs (Conj e1 e2)      = eval vs e1 && eval vs e2
eval vs (Disj e1 e2)      = eval vs e1 || eval vs e2
eval vs (Implies e1 e2)   = not (eval vs e1) || eval vs e2
eval vs (Iff e1 e2)       = eval vs e1 == eval vs e2
eval vs (Top)             = True
eval vs (Bottom)          = False

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
    go (Top)           !vs = vs
    go (Bottom)        !vs = vs

-- | Negation normal form.
-- (May result in exponential growth)
nnf :: Expr -> Expr
nnf = transformDown nnf1 . propConst

nnf1 :: Expr -> Expr
nnf1 ex = case ex of
  Neg (Neg e) -> nnf1 e
  Neg (Conj e1 e2) -> Neg e1 `Disj` Neg e2
  Neg (Disj e1 e2) -> Neg e1 `Conj` Neg e2
  Implies e1 e2 -> Neg e1 `Disj` e2
  Neg (Implies e1 e2) -> e1 `Conj` Neg e2
  Iff e1 e2 -> let a = e1 `Disj` Neg e2
                   b = Neg e1 `Disj` e2
               in a `Conj` b
  Neg (Iff e1 e2) -> let a = e1 `Disj` e2
                         b = Neg e1 `Disj` Neg e2
                     in a `Conj` b
  e -> e

-- | Conjunctive normal form.
-- (May result in exponential growth)
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
simp = transformUp (propConst1 . simp1)

simp1 :: Expr -> Expr
simp1 ex = case ex of
  Disj e1 (Neg e2) | e1 == e2 -> Top
  Disj (Neg e1) e2 | e1 == e2 -> Top
  Conj e1 e2       | e1 == e2 -> e1
  e -> e

-- | Test if expression is constant.
isConst :: Expr -> Bool
isConst Top = True
isConst Bottom = True
isConst e = False

-- | Transform expression up from the bottom.
transformUp :: (Expr -> Expr) -> Expr -> Expr
-- TODO: This could probably be done with Data.Data, but it's outside my capabilities for now. 
transformUp f ex = case ex of
  Neg e -> f $ Neg (transformUp f e)
  Conj e1 e2 -> f $ Conj (transformUp f e1) (transformUp f e2)
  Disj e1 e2 -> f $ Disj (transformUp f e1) (transformUp f e2)
  Iff e1 e2 -> f $ Iff (transformUp f e1) (transformUp f e2)
  Implies e1 e2 -> f $ Implies (transformUp f e1) (transformUp f e2)
  e -> f e

-- | Transform expression down from the top.
transformDown :: (Expr -> Expr) -> Expr -> Expr
-- TODO: This could probably be done with Data.Data, but it's outside my capabilities for now. 
transformDown f ex = case f ex of
  Neg e -> Neg (transformDown f e)
  Conj e1 e2 -> Conj (transformDown f e1) (transformDown f e2)
  Disj e1 e2 -> Disj (transformDown f e1) (transformDown f e2)
  Iff e1 e2 -> Iff (transformDown f e1) (transformDown f e2)
  Implies e1 e2 -> Implies (transformDown f e1) (transformDown f e2)
  e -> e

-- | Propagate constants (to simplify expression).
propConst :: Expr -> Expr
propConst = transformUp propConst1

propConst1 :: Expr -> Expr
propConst1 ex = case ex of
  Neg (Neg e) -> e
  Neg Top -> Bottom
  Neg Bottom -> Top
  Conj Top e2 -> e2
  Conj Bottom e2 -> Bottom
  Conj e1 Top -> e1
  Conj e1 Bottom -> Bottom
  Disj Top e2 -> Top
  Disj Bottom e2 -> e2
  Disj e1 Top -> Top
  Disj e1 Bottom -> e1
  Iff Top Bottom -> Bottom
  Iff Bottom Top -> Bottom
  Iff Bottom Bottom -> Top
  Iff Top e2 -> e2
  Iff Bottom e2 -> Neg e2
  Iff e1 Top -> e1
  Iff e1 Bottom -> Neg e1
  Implies Top e2 -> e2
  Implies Bottom e2 -> Top
  Implies e1 Top -> Top
  Implies e1 Bottom -> Neg e1
  e -> e
