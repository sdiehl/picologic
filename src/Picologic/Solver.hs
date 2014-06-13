module Picologic.Solver (
  solveProp,
  clausesExpr,
) where

import Debug.Trace

import Picologic.AST
import Picosat

import Data.List
import qualified Data.Map as M
import Control.Monad.Writer

data Clause
  = CV Int       -- ^Clause variable ( a or -a )
  | CL [Clause]  -- ^Set of clause under disjuntion

instance Show Clause where
  show (CV i) = show i
  show (CL xs) = concat (intersperse " " (fmap show xs))

-- | Yield the soutions for an expressions using the PicosSAT solver.
solveProp :: Expr -> IO Solutions
solveProp p = solveAll cs >>= return . Solutions . fmap (backSubst vs')
  where
    cs = filter (not . null) $ fmap toInts $ execWriter $ clauses vs (cnf p)
    vs  = M.fromList $ zip vars [1..]
    vs' = M.fromList $ zip [1..] vars
    vars = variables (cnf p)

-- | Yield the integer clauses given to the SAT solver.
clausesExpr :: Expr -> [[Int]]
clausesExpr p = filter (not . null) $ fmap toInts $ execWriter $ clauses vs (cnf p)
  where
    vs = M.fromList $ zip vars [1..]
    vars = variables (cnf p)

backSubst :: M.Map Int Ident -> Solution -> [Expr]
backSubst env (Solution xs) = fmap go xs
  where
    go x | x >= 0 = Var (env M.! x)
    go x | x < 0 = Neg (Var (env M.! (abs x)))
backSubst _ Unsatisfiable = []
backSubst _ Unknown = []

toInts :: Clause -> [Int]
toInts (CL xs) = fmap (\(CV n) -> n) xs
toInts (CV x) = [x]

neg :: Clause -> Clause
neg (CV n) = CV (-n)
neg (CL xs) = CL (fmap neg xs)

combine :: Clause -> Clause -> Clause
combine (CL x) (CL y) = CL (x++y)
combine (CL x) y = combine (CL x) (CL [y])
combine x (CL y) = combine (CL [x]) (CL y)
combine x y = CL [x, y]

clauses :: M.Map Ident Int -> Expr -> Writer [Clause] Clause
clauses env ex = case ex of
  Var v -> return $ CV (env M.! v)
  Neg x -> do
    cs <- clauses env x
    return (neg cs)
  Conj e1 e2 -> do
    cs1 <- clauses env e1
    cs2 <- clauses env e2
    tell [cs1, cs2]
    return (CL [])
  Disj e1 e2 -> do
    cs1 <- clauses env e1
    cs2 <- clauses env e2
    return (combine cs1 cs2)
