data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add l r) = (eval l) + (eval r)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add l r) = (printExpr l) ++ " + " ++ (printExpr r)
