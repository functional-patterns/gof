
data Expression = Literal Literal | Operation Operation deriving (Show, Eq)
data Literal = One | Two | Three deriving (Show, Eq)
data Operation = Plus Expression Expression | Minus Expression Expression deriving (Show, Eq)

evaluateExpression :: Expression -> Int
evaluateExpression (Literal l) = evaluateLiteral l
evaluateExpression (Operation o) = evaluateOperation o

evaluateLiteral :: Literal -> Int
evaluateLiteral One = 1
evaluateLiteral Two = 2
evaluateLiteral Three = 3

evaluateOperation :: Operation -> Int
evaluateOperation (Plus a b) = (evaluateExpression a) + (evaluateExpression b)
evaluateOperation (Minus a b) = (evaluateExpression a) - (evaluateExpression b)

main :: IO ()
main = do
    let one = Literal One
    let two = Literal Two
    let three = Literal Three

    let sum = Operation $ Plus two three
    let expression = Operation $ Minus sum one

    putStrLn $ show (evaluateExpression expression)
