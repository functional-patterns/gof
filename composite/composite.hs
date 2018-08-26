--
-- COMPOSITE
--
-- Compose objects into tree structures to represent part-whole hierarchies. Composite lets clients
-- treat individual objects and compositions of objects uniformly.
--

--
-- Example
-- 
-- In this simple example a discriminated union is used to present a shape. A shap may be a single
-- circle, single square or composite (aka any combination of those).
-- 
-- Client code may use any of the functions supporting the shape type without knowing if the shape
-- is a single item or a more complex composition.
-- 

-- Helper types to indicate what field is defined
type Position = (Float, Float)
type Radius = Float
type Size = Float

-- Simple disjoint union to present Shape
data Shape = Circle Position Radius | Square Position Size | Composite [Shape] deriving (Show)

-- Function to scale a shape
scale :: Shape -> Float -> Shape
scale (Circle position radius) factor =
    Circle position (radius * factor)
scale (Square position size) factor =
    Square position (size * factor)
scale (Composite shapes) factor =
    Composite (map (\s -> scale s factor) shapes)

-- Function to move a shape
move :: Shape -> Float -> Float -> Shape
move (Circle position radius) deltaX deltaY =
    Circle (fst position + deltaX, snd position + deltaY) radius
move (Square position size) deltaX deltaY =
    Square (fst position + deltaX, snd position + deltaY) size
move (Composite shapes) deltaX deltaY =
    Composite (map (\s -> move s deltaX deltaY) shapes)

-- Function to calculate area of a shape
area :: Shape -> Float
area (Circle _ radius) =
    3.14 * radius * radius
area (Square _ size) =
    size * size
area (Composite shapes) =
    foldl (\a s -> a + area s) 0.0 shapes

-- Tester function to demonstrate usage
main :: IO ()
main = do
    let (|>) a f = f a

    let original = Composite [Circle (0, 0) 2, Composite [Square (2, 2) 1, Square (1, 4) 4]]
    let scaled = scale original 2.0
    let moved = move original 1.0 1.0

    original |> show |> ("original " ++) |> putStrLn
    original |> area |> show |> ("original area = " ++ ) |> putStrLn
    scaled   |> show |> ("scaled " ++) |> putStrLn
    scaled   |> area |> show |>  ("scaled area = " ++) |> putStrLn
