import Data.Char (isDigit, isSpace)

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

listToArguments :: [Expr] -> Expr
listToArguments [] = Null
listToArguments (first:rest) = Pair first (listToArguments rest)


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


extractOperator:: String -> String
extractOperator [] = []
extractOperator (_ : xs) = collect xs
    where collect [] = []
          collect (x:xs) | isSpace x = []
                         | otherwise = x : collect xs

extractArguments :: String -> (String, String)
extractArguments [] = ([],[])
extractArguments input = collect input ""
    where
        collect :: String -> String -> (String, String)
        collect []  collected = (reverse collected, [])
        collect (x:xs) collected | isSpace x || x == ')'= (reverse collected,xs)
                                 | otherwise =  collect xs (x : collected)

parseArguments :: String -> [Expr]
parseArguments [] = []
parseArguments (')':_) = []
parseArguments input = parse argument : parseArguments rest
    where
        (argument, rest) = extractArguments input
