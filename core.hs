data Expr = Number Integer

showExpr :: Expr -> String
showExpr (Number n) = show n
