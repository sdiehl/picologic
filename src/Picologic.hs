{- |

Example usage:

@
import Picologic
p, q, r :: Expr
p = readExpr \"~(A | B)\"
q = readExpr \"(A | ~B | C) & (B | D | E) & (D | F)\"
r = readExpr \"(φ \<-\> ψ)\"
@

The expression can be pretty printed using logical symbols:

>>> ppExprU p
¬(A ∨ B)
>>> ppExprU q
((((A ∨ ¬B) ∨ C) ∧ ((B ∨ D) ∨ E)) ∧ (D ∨ F))
>>> ppExprU (cnf r)
((φ ∧ (φ ∨ ¬ψ)) ∧ ((ψ ∨ ¬φ) ∧ ψ))

To run the expression against the SAT solver run:

>>> solveProp p >>= putStrLn . ppSolutions
~A ~B
~A B
A ~B

-}

module Picologic (
  module Picologic.AST,
  module Picologic.Parser,
  module Picologic.Solver,
  module Picologic.Pretty,
) where

import Picologic.AST
import Picologic.Parser
import Picologic.Solver
import Picologic.Pretty
