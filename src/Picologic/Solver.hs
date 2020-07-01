module Picologic.Solver (
  solveProp,
  solveCNF,
  solveOneCNF,
  clausesExpr,
  addVarsToSolutions
) where

import Picologic.AST
import Picologic.Pretty
import Picosat

import Data.List
import Data.Maybe(mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Writer

-- | Yield the solutions for an expression using the PicoSAT solver.
solveProp :: Expr -> IO Solutions
solveProp p = solveCNF $ cnf p

-- | Yield the solutions for an expression using the PicoSAT
-- solver. The Expression must be in CNF form already.
solveCNF :: Expr -> IO Solutions
solveCNF p = if isConst p
             then
               return $ if eval M.empty p
                        then Solutions [[]]
                        else Solutions []
             else do
                 solutions <- solveAll ds
                 return $ Solutions $ mapMaybe (backSubst vs') solutions
  where
    cs = clausesFromCNF p
    ds = cnfToDimacs vs cs
    vs  = M.fromList $ zip vars [1..]
    vs' = M.fromList $ zip [1..] vars
    vars = variables p

-- | Yield one single solution for an expression using the PicoSAT
-- solver. The Expression must be in CNF form already.
solveOneCNF :: Expr -> IO (Maybe [Expr])
solveOneCNF p = if isConst p
                then
                  return $ if eval M.empty p
                           then Just []
                           else Nothing
                else do
                  solution <- solve ds
                  return $ backSubst vs' solution
  where
    cs = clausesFromCNF p
    ds = cnfToDimacs vs cs
    vs  = M.fromList $ zip vars [1..]
    vs' = M.fromList $ zip [1..] vars
    vars = variables p

clausesFromCNF :: Expr -> [[Expr]]
clausesFromCNF p = [ [ case lit of
                       v@(Var name) -> v
                       v@(Neg (Var name)) -> v
                       x -> error $ "input not in CNF: \n" ++ show p
                     | lit <- ors [clause] ]
                   | clause <- ands [p]]

ands :: [Expr] -> [Expr]
ands [] = []
ands (Conj a b : xs) = ands [a] ++ ands [b] ++ ands xs
ands (x:xs) = x : ands xs

ors :: [Expr] -> [Expr]
ors [] = []
ors (Disj a b : xs) = ors [a] ++ ors [b] ++ ors xs
ors (x:xs) = x : ors xs

cnfToDimacs :: M.Map Ident Int -> [[Expr]] -> [[Int]]
cnfToDimacs vs = map (map encode)
  where encode (Var ident)       = vs M.! ident
        encode (Neg (Var ident)) = negate $ vs M.! ident


-- | Yield the integer clauses given to the SAT solver.
clausesExpr :: Expr -> [[Int]]
clausesExpr p = ds
  where
    cs = clausesFromCNF p
    vs  = M.fromList $ zip vars [1..]
    vars = variables p
    ds = cnfToDimacs vs cs

backSubst :: M.Map Int Ident -> Solution -> Maybe [Expr]
backSubst env (Solution xs) = Just $ fmap go xs
  where
    go x | x >= 0 = Var (env M.! x)
    go x | x < 0 = Neg (Var (env M.! abs x))
backSubst _ Unsatisfiable = Nothing
backSubst _ Unknown = Nothing

addVarsToSolutions :: [Ident] -> Solutions -> Solutions
addVarsToSolutions vars (Solutions sols) = Solutions $ concatMap addVarsToSolution sols
  where
    addVarsToSolution sol = map (sol ++) $ sequence  [ [Var v, Neg(Var v)] | v <- newVars $ head sols ]
    newVars sol = vars \\ concatMap variables sol
