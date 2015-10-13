import Picologic

main = print $ ppExprLisp d
  where a =
          Disj (Var (Ident "some-var"))
               (Disj (Var (Ident "another-var"))
                     (Iff (Var (Ident "ggg"))
                          (Neg (Iff (Neg (Var (Ident "var-x")))
                                    (Var (Ident "origin"))))))
        b = 
          Conj (Var (Ident "var-y"))
               (Conj (Var (Ident "ccc"))
                     (Iff (Var (Ident "ddddd"))
                          (Neg (Iff (Neg (Var (Ident "abcdefg")))
                                    (Var (Ident "ggg"))))))
        c = Disj a b
        d = Conj c c
