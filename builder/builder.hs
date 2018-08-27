--
-- BUILDER
-- 
-- Separate the construction of a complex object from its representation so that the same
-- construction process can create different representations. 
-- 

--
-- Example
-- 
-- This example demonstrates how common builder can be used to create documents in different
-- formats.
--

(|>) a f = f a

-- Some type to be used to build documents from
data Book = Book { title :: String, author :: String, year :: Int }

-- Some function types required by the builder
type TitleFunction d = String -> d -> d
type AuthorFunction d = String -> d -> d
type YearFunction d = Int -> d -> d

-- Builder template function to create different kind of documents
builderTemplate :: d -> (TitleFunction d) -> (AuthorFunction d) -> (YearFunction d) -> Book -> d
builderTemplate createDocument addTitle addAuthor addYear book = do
    createDocument |> addTitle (title book)
                   |> addAuthor (author book)
                   |> addYear (year book)


-- Simple document type and functions to build it
type SimpleDocument = String

simpleTitle :: TitleFunction SimpleDocument
simpleTitle title document = document ++ "title: " ++ title ++ "\n"

simpleAuthor :: String -> SimpleDocument -> SimpleDocument
simpleAuthor name document = document ++ "author: " ++ name ++ "\n"

simpleYear :: Int -> SimpleDocument -> SimpleDocument
simpleYear year document = document ++ "year: " ++ (show year) ++ "\n"

simpleShow :: SimpleDocument -> String
simpleShow document = document


-- XML document type and functions to build it
data XmlDocument = XmlDocument { tag :: String, elements :: [String] }

xmlTitle :: TitleFunction XmlDocument
xmlTitle title (XmlDocument tag elements) =
    XmlDocument tag (elements ++ ["<title>" ++ title ++ "</title>"])

xmlAuthor :: AuthorFunction XmlDocument
xmlAuthor author (XmlDocument tag elements) =
    XmlDocument tag (elements ++ ["<author>" ++ author ++ "</author>"])

xmlYear :: YearFunction XmlDocument
xmlYear year (XmlDocument tag elements) =
    XmlDocument tag (elements ++ ["<year>" ++ (show year) ++ "</year>"])

xmlShow :: XmlDocument -> String
xmlShow (XmlDocument tag elements) = do
    "<" ++ tag ++ ">" ++ (concat elements) ++ "</" ++ tag ++ ">"

-- Tester function
main :: IO ()
main = do

    let book = Book "Advanced Haskell" "Lawrence, Jennifer" 2018
    
    let xmlBuilder = builderTemplate (XmlDocument "book" []) xmlTitle xmlAuthor xmlYear
    let xml = xmlBuilder book

    let simpleBuilder = builderTemplate "" simpleTitle simpleAuthor simpleYear
    let simple = simpleBuilder book

    putStrLn $ xmlShow xml
    putStrLn $ simpleShow simple
