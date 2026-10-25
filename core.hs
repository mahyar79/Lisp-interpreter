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

applyPrimitive :: Expr -> Expr -> Expr
applyPrimitive (PrimitiveProcedure function) arguments = function (argumentsToList arguments)

showExpr :: Expr -> String
showExpr Null = ""
showExpr (Number n) = show n
showExpr (Variable name) = name
showExpr (Pair first rest) = "("++ showExpr first++" "++unwords (map showExpr (argumentsToList rest))++")"

eval :: Environment -> Expr -> Expr
eval environment (Number number) = Number number
eval environment (Variable variable) = lookUpValue environment variable
eval environment (Pair (Variable name) arguments) = applyPrimitive (lookUpValue environment name) arguments

parse :: String -> Expr
parse x = Number(read x)
