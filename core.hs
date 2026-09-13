data Expr = Number Integer
          | Pair Expr Expr
          | Variable String
          | Null

type Environment = [(String, Expr)]

lookUpValue :: Environment -> String -> Expr
lookUpValue environment name = go environment
  where
    go [] = Null
    go ((symbol, result) : rest)
      | symbol == name = result
      | otherwise      = go rest

showExpr :: Expr -> String
showExpr (Number n) = show n

eval :: Expr -> Integer
eval (Number n) = n
