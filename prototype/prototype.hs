
-- Common type to be required for all clonable objects
type Position = (Float, Float)


-- Music module
data Note = HalfNote Position | FullNote Position deriving Show

noteInsertionTool :: Note -> Position -> Note
noteInsertionTool (HalfNote _) position = HalfNote position
noteInsertionTool (FullNote _) position = FullNote position


-- Graphics module
data Shape = Circle Float Position | Square Float Position deriving Show

shapeInsertionTool :: Shape -> Position -> Shape
shapeInsertionTool (Circle r _) position = Circle r position
shapeInsertionTool (Square s _) position = Square s position


main :: IO ()
main = do
    putStrLn "prototype"


    -- create some music
    let fullNoteInsertionTool = noteInsertionTool (HalfNote (0, 0))
    let halfNoteInsertionTool = noteInsertionTool (FullNote (0, 0))
    
    let music = [fullNoteInsertionTool (2, 5), halfNoteInsertionTool (1, 1) ]

    putStr "music: "
    putStrLn $ show music


    -- create some art
    let circleInsertionTool = shapeInsertionTool (Circle 2 (0, 0))
    let squareInsertionTool = shapeInsertionTool (Square 4 (0, 0))
    
    let art = [circleInsertionTool (2, 5), squareInsertionTool (1, 1) ]
    
    putStr "art: "
    putStrLn $ show art
