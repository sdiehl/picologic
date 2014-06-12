module Main where

import Picologic

p, q, r :: Expr
p = readExpr "~(A | B)"
q = readExpr "(A | ~B | C) & (B | D | E) & (D | F)"
r = readExpr "(φ <-> ψ)"

ps = ppExprU p
-- ¬(A ∨ B)
qs = ppExprU q
-- ((((A ∨ ¬B) ∨ C) ∧ ((B ∨ D) ∨ E)) ∧ (D ∨ F))
rs = ppExprU (cnf r)
-- ((φ ∧ (φ ∨ ¬ψ)) ∧ ((ψ ∨ ¬φ) ∧ ψ))

main :: IO ()
main = solveProp p >>= putStrLn . ppSolutions
-- ¬A ¬B
-- ¬A B
-- A ¬B
