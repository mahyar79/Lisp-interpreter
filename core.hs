data Expr = Number Integer
          | Pair Expr Expr
          | Variable String
          | Null
          | PrimitiveProcedure ([Expr] -> Expr)

type Environment = [(String, Expr)]

lookUpValue :: Environment -> String -> Expr
lookUpValue environment name = go environment
  where
    go [] = Null
    go ((symbol, result) : rest)
      | symbol == name = result
      | otherwise      = go rest

primitives :: Environment
primitives = [("+", PrimitiveProcedure addNumbers)]

addNumbers :: [Expr] -> Expr
addNumbers ((Number num1) : (Number num2) : []) = Number (num1 + num2)

argumentsToList :: Expr -> [Expr]
argumentsToList Null = []
argumentsToList (Pair first rest) = first : argumentsToList rest

showExpr :: Expr -> String
showExpr (Number n) = show n

eval :: Environment -> Expr -> Expr
eval environment (Number number) = Number number
eval environment (Variable variable) = lookUpValue environment variable
