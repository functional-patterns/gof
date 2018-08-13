
data Expression = Literal Literal | Operation Operation deriving (Show, Eq)
type Literal = Int
data Operation = Plus Expression Expression | Minus Expression Expression deriving (Show, Eq)

evaluateExpression :: Expression -> Literal
evaluateExpression (Literal l) = evaluateLiteral l
evaluateExpression (Operation o) = evaluateOperation o

evaluateLiteral :: Literal -> Literal
evaluateLiteral = id

evaluateOperation :: Operation -> Literal
evaluateOperation (Plus a b) = (evaluateExpression a) + (evaluateExpression b)
evaluateOperation (Minus a b) = (evaluateExpression a) - (evaluateExpression b)

main :: IO ()
main = do
    let one = Literal 1
    let two = Literal 2
    let three = Literal 3

    let sum = Operation $ Plus two three
    let expression = Operation $ Minus sum one

    putStrLn $ show (evaluateExpression expression)
