import Picologic.AST
import Picologic.Tseitin
import Picologic.Solver
import Picologic.Pretty

import Test.QuickCheck
import Data.List
import qualified Data.Map as M
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

instance Arbitrary Expr where
  arbitrary = sized $ \n ->
                tree (round $ sqrt $ fromIntegral n :: Int)
    where tree 0 = elements $ map (Var . Ident) (concat $ replicate 3 ["a", "b", "c", "d"]) ++ [Top, Bottom]
          tree n =
            oneof
            [do a <- tree (pred n)
                return $ Neg a,
             do l <- arbitrary
                let n2 = l `mod` n
                a <- tree n2
                b <- tree (n-n2)
                con <- elements [Conj, Disj, Implies, Iff]
                return $ con a b]
  shrink (Var _) = []
  shrink (Neg (Neg x)) = [x, Neg x] ++ map Neg (shrink x)
  shrink (Neg x) = [x] ++ map Neg (shrink x)
  shrink (Conj a b) = [a, b]
                     ++ map (Conj a) (shrink b)
                     ++ map (\aa-> Conj aa b) (shrink a)
  shrink (Disj a b) = [a, b]
                     ++ map (Disj a) (shrink b)
                     ++ map (\aa-> Disj aa b) (shrink a)
  shrink (Implies a b) = [a, b]
                     ++ map (Implies a) (shrink b)
                     ++ map (\aa-> Implies aa b) (shrink a)
  shrink (Iff a b) = [a, b]
                     ++ map (Iff a) (shrink b)
                     ++ map (\aa-> Iff aa b) (shrink a)
  shrink Top = []
  shrink Bottom = []


env = M.fromList [(Ident "a", True),
                  (Ident "b", True),
                  (Ident "c", False),
                  (Ident "d", False)]

test_nnf :: Expr -> Bool
test_nnf e = eval env e == eval env (nnf e)

test_cnf :: Expr -> Bool
test_cnf e = eval env e == eval env (cnf e)

test_tseitin :: Expr -> Bool
test_tseitin e = unsafePerformIO test
  where test = do
          let vars = variables e
          let ts = tseitinCNF e
          -- putStrLn "\nexpr"
          -- print $ ppExprLisp e
          -- putStrLn "tseitin"
          -- print $ ppExprLisp ts
          -- putStrLn "tseitin clauses"
          -- mapM_ print $ clausesExpr ts
          let ce = cnf e
          as <- solveCNF ce
          bs0 <- solveCNF ts
          let bs =  dropTseitinVarsInSolutions bs0
          let as1 = addVarsToSolutions vars as
          let bs1 = addVarsToSolutions vars bs
          --print ("as", as)
          --print ("bs", bs)
          return $ normalize as1 == normalize bs1
        normalize (Solutions ssv) = sort $ map sort ssv

qc = verboseCheckWith (stdArgs { maxSuccess = 2000 })

-- how to make an error fail a 'cabal test'?
qcwf p = verboseCheckWith (stdArgs { maxSuccess = 1000 })
         (whenFail exitFailure p)

main = do
  putStrLn "nnf"
  qc test_nnf
  putStrLn "cnf"
  qc test_cnf
  putStrLn "tseitin"
  qc test_tseitin
