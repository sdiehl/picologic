Picologic
---------

[![Build Status](https://travis-ci.org/sdiehl/picologic.svg?branch=master)](https://travis-ci.org/sdiehl/picologic)

Picologic is a lightweight library for working with symbolic logic expresisons. It is built against the
picosat Haskell library which bundles the SAT solver with the Haskell package so no external solver or
dependencies are neccessary.

Picologic provides the logic expressions, parser and normal form conversion functions to express the logic
expressions more naturally and then feed them to the SAT solver.

Installing
----------

To install just the library:

```bash
$ cabal install picologic
```

To build the interactive shell use:

```bash
$ cabal get picologic
$ cd picologic-0.1
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
```

The expression AST consists just of the logical connectives. 

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
  deriving (Eq, Ord, Show, Data, Typeable)
```

To use the interactive shell when compiled with with ``-fshell`` invoke picologic at the shell.

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
Copyright (c) 2014-2016, Stephen Diehl
