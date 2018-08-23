--
-- TEMPLATE METHOD
-- 
-- Define the skeleton of an algorithm in an operation, deferring some steps to subclasses.
-- Template Method lets subclasses redefine certain steps of an algorithm without changing the
-- algorithm's structure.
-- 

--
-- Example
--
-- This example defines a template function for compressing files. The template enforces the generic
-- structure of how compression is done as well as the general structure of the compressed file.
--
--   format | size | checksum | <compressed data>

-- The altering parts are compression and checksum algorithsm, which are given as parameters for the
-- template compression function.
-- 


type CompressFunction = [Char] -> [Char]
type ChecksumFunction = [Char] -> Int

zipTemplate :: CompressFunction -> ChecksumFunction -> String -> [Char] -> [Char]
zipTemplate compressAlgorithm checksumAlgorithm format input = do
    let payload = compressAlgorithm input
    let size = length payload
    let checksum = checksumAlgorithm input

    format ++ " | " ++ (show size) ++ " | " ++ (show checksum) ++ " | " ++ payload

--
-- Simple compress algorithm removes spaces
-- 
simpleCompress :: CompressFunction
simpleCompress input =
    filter (\x -> x /= ' ')  input

--
-- Simple checksum is calculated by xorring all the bytes in the data
-- 
simpleChecksum :: ChecksumFunction
simpleChecksum input =
    length input


main :: IO ()
main = do
    let phrase = "a brown fox jumped over the lazy dog"

    -- Partial application is used to the template method create a concrete zipper function
    let simpleZip = zipTemplate simpleCompress simpleChecksum "SIMPLE"
    let compressed =  simpleZip phrase

    putStrLn compressed
