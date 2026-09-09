data Expr = Number Integer
          | Pair Expr Expr
          | Variable String
          | Null


showExpr :: Expr -> String
showExpr (Number n) = show n

eval :: Expr -> Integer
eval (Number n) = n
