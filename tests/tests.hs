import Test.QuickCheck
import Picologic.AST
import qualified Data.Map as M

instance Arbitrary Expr where
  arbitrary =
    frequency
    [ (10, elements $ map (Var . Ident) ["a", "b", "c", "d"]),
      (8, do a <- arbitrary
             return $ Neg a),
      (2, do a <- arbitrary
             b <- arbitrary
             con <- elements [ Conj, Disj, Implies, Iff ]
             return $ con a b)]
  shrink (Var _) = []
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
                       -- map (`Conj` b) (shrink a),
                       -- map (Conj a) (shrink b)]


env = M.fromList [(Ident "a", True),
                  (Ident "b", True)]

test_nnf :: Expr -> Bool
test_nnf e = eval env e == eval env (nnf e)

test_cnf :: Expr -> Bool
test_cnf e = eval env e == eval env (cnf e)

qc = quickCheckWith $ stdArgs { maxSuccess = 1000 }

main =
  do putStrLn "nnf"
     qc test_nnf
     putStrLn "cnf"
     qc test_cnf
