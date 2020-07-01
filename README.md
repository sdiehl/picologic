Picologic
---------

[![Build Status](https://travis-ci.org/sdiehl/picologic.svg?branch=master)](https://travis-ci.org/sdiehl/picologic)

Picologic is a lightweight library for working with symbolic logic expressions.
It is built against the picosat Haskell library which bundles the SAT solver
with the Haskell package so no external solver or dependencies are necessary.

Picologic provides the logic expressions, parser and normal form conversion, and
Tseytin transformations to express the logic expressions in equational form and
generate constraint sets for use in SAT/SMT solvers.

Installing
----------

To use the library using Stack use:

```bash
$ git clone git@github.com:sdiehl/picologic.git
$ cd picologic
$ stack build
$ stack test
```

Or using Cabal:

```bash
$ cabal install picologic
```

To build the interactive shell compile with the ``-fshell``  flag:

```bash
$ cabal get picologic
$ cd picologic-0.2.0
$ cabal configure -fshell
$ cabal install
```

Usage
-----

To use the API import the ``Picologic`` module.

```haskell
import Picologic

p, q, r :: Expr
p = readExpr "~(A | B)"
q = readExpr "(A | ~B | C) & (B | D | E) & (D | F)"
r = readExpr "(φ <-> ψ)"
s = readExpr "(0 | A) -> (A & 1)"

ps = ppExprU p
-- ¬(A ∨ B)
qs = ppExprU q
-- ((((A ∨ ¬B) ∨ C) ∧ ((B ∨ D) ∨ E)) ∧ (D ∨ F))
rs = ppExprU (cnf r)
-- ((φ ∧ (φ ∨ ¬ψ)) ∧ ((ψ ∨ ¬φ) ∧ ψ))
ss = ppExprU s
-- ((⊥ ∨ A) → (A ∧ ⊤))
ss1 = ppExprU (cnf s)
-- ⊤

main :: IO ()
main = solveProp p >>= putStrLn . ppSolutions
-- ¬A ¬B
-- ¬A B
-- A ¬B
```

The expression AST consists just of the logical connectives or constants. 

```haskell
newtype Ident = Ident String
  deriving (Eq, Ord, Show, Data, Typeable)

data Expr
  = Var       Ident      -- ^ Variable
  | Neg       Expr       -- ^ Logical negation
  | Conj      Expr Expr  -- ^ Logical conjunction
  | Disj      Expr Expr  -- ^ Logical disjunction
  | Iff       Expr Expr  -- ^ Logical biconditional
  | Implies   Expr Expr  -- ^ Material implication
  | Top                  -- ^ Constant true
  | Bottom               -- ^ Constant false
  deriving (Eq, Ord, Show, Data, Typeable)
```

To use the interactive shell when compiled with with ``-fshell`` invoke
`picologic` at the shell.

```bash
$ picologic
Picologic 0.1
Type :help for help

Logic> p | q
(p ∨ q)
Solutions:
p q
¬p q
p ¬q

Logic> ~(a -> (b <-> c))
(a ∧ ((b ∨ c) ∧ (¬b ∨ ¬c)))
Solutions:
¬a ¬b c
a b ¬c
¬a b ¬c
a ¬b c

Logic> :clauses
[[2,3],[-2,-3],[2,3,-2,-3],[1,2,3,-2,-3]]
```

License
-------

Released under the MIT License.
Copyright (c) 2014-2020, Stephen Diehl
