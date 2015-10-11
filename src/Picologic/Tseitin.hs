module Picologic.Tseitin
       (tseitinCNF,
        dropTseitinVarsInSolutions,
        dropTseitinVars,
        
       ) where

-- TODO: How efficient is the `mappend` used by Writer?
-- TODO: For cases like (Conj (Conj a b) c) the introduction
--       of one Tseitin var can be enough.
-- TODO: The outermost (Conj (Conj ...) ...) needn't be
--       Tseitin encoded at all.

import Prelude hiding (or, and)

import Picologic.AST
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

type TS a = StateT Int (Writer [Expr]) a

evalTS :: TS a -> (a, [Expr])
evalTS action = 
  runWriter (evalStateT action 1)

var :: TS Expr
var = do
  n <- get
  put $ succ n
  return $ Var $ Ident $ "ts*" ++ show n

or xs = foldl1 Disj xs
and xs = foldl1 Conj xs

tseitinCNF :: Expr -> Expr
tseitinCNF e =
  let (var, clauses) = evalTS $ tseitin $ simplify e
  in and (var : clauses)

neg (Neg x) = x
neg x       = Neg x

tseitin :: Expr -> TS Expr

tseitin lit@(Var _) = return lit

tseitin lit@(Neg (Var _)) = return lit

tseitin (Conj x y) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [neg a, neg b, c],
        or [a, neg c],
        or [b, neg c]]
  return c

tseitin (Neg (Conj x y)) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [neg a, neg b, neg c],
        or [a, c],
        or [b, c]]
  return c

tseitin (Disj x y) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [a, b, neg c],
        or [neg a, c],
        or [neg b, c]]
  return c

tseitin (Neg (Disj x y)) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [a, b, c],
        or [neg a, neg c],
        or [neg b, neg c]]
  return c


-- |
-- @
-- (c -> (a -> b)) & (-c -> -(a->b))
-- (-c | (-a | b)) & (c | (a&-b))
-- (-a|b|-c) & (a|c) & (-b|c)
-- @
tseitin (Implies x y) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [neg a, b, neg c],
        or [a, c],
        or [neg b, c]]
  return c

-- |
-- @
-- (c -> -(a -> b)) & (-c -> (a->b))
-- (-c | (a&-b)) & (c | (-a|b))
-- (a|-c) & (-b|-c) & (-a|b|c)
-- @
tseitin (Neg (Implies x y)) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [a, neg c],
        or [neg b, neg c],
        or [neg a, b, c]]
  return c

-- |
-- @
-- (c -> a == b) & (-c -> a /= b)
-- (-c | ((-a|b) & (a|-b))) & (c | ((a|b) & (-a|-b)))
-- (-a|b|-c) & (a|-b|-c) & (a|b|c) & (-a|-b|c)
-- @
tseitin (Iff x y) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [neg a, b, neg c],
        or [a, neg b, neg c],
        or [a, b, c],
        or [neg a, neg b, c]]
  return c

-- |
-- @
-- (c -> a /= b) & (-c -> a == b)
-- (-c | ((a|b) & (-a|-b))) & (c | ((-a|b) & (a|-b)))
-- (a|b|-c) & (-a|-b|-c) & (-a|b|c) & (a|-b|c)
-- @
tseitin (Neg (Iff x y)) = do
  a <- tseitin x
  b <- tseitin y
  c <- var
  tell [or [a, b, neg c],
        or [neg a, neg b, neg c],
        or [neg a, b, c],
        or [a, neg b, c]]
  return c

tseitin (Neg x) = do
  a <- tseitin x
  c <- var
  tell [or [neg a, neg c],
        or [a, c]]
  return c

dropTseitinVarsInSolutions (Solutions xs) =
  Solutions $ map dropTseitinVars xs

dropTseitinVars :: [Expr] -> [Expr]
dropTseitinVars = filter (\x -> not $ isTseitinLiteral x)

isTseitinLiteral :: Expr -> Bool
isTseitinLiteral lit =
  case lit of
    (Var (Ident nm)) -> tseitinName nm
    (Neg (Var (Ident nm))) -> tseitinName nm

tseitinName ('t':'s':'*':_) = True
tseitinName _               = False

simplify :: Expr -> Expr
simplify (Neg (Neg x)) = simplify x
simplify v@(Var _) = v
simplify (Neg a) = neg $ simplify a
simplify (Conj a b) = Conj (simplify a) (simplify b)
simplify (Disj a b) = Disj (simplify a) (simplify b)
simplify (Implies a b) = Implies (simplify a) (simplify b)
simplify (Iff a b) = Iff (simplify a) (simplify b)
                        
