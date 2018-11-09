data CriminalRecord = CriminalRecord { name :: String, crime :: String } deriving (Show)

-- Core function to lookup criminal records
criminalRecord :: String -> Maybe CriminalRecord
criminalRecord ssn = do
    let criminals = [ ("JENN", CriminalRecord "Jennifer Lawrence" "bad acting"),
                      ("HILL", CriminalRecord "Hillary Clinton" "treason"),
                      ("OBAM", CriminalRecord "Barak Obama" "treason"),
                      ("HOUD", CriminalRecord "Harry Houdini" "escaping") ]

    lookup ssn criminals


-- FBI has access to see all crimes
fbiCriminalRecord :: String -> Maybe CriminalRecord
fbiCriminalRecord = criminalRecord


-- Police has access to see only crimes of ordinary people
policeCriminalRecord :: String -> Maybe CriminalRecord
policeCriminalRecord ssn = do
    let allowed = ["JENN", "HOUD"]

    if elem ssn allowed
    then criminalRecord ssn
    else Nothing


main :: IO ()
main = do
    putStrLn "proxy"

    print $ policeCriminalRecord "JENN"
    print $ policeCriminalRecord "HILL"
    print $ fbiCriminalRecord "HILL"
