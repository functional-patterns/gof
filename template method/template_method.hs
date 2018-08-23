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
-- This example defines a template method for compressing files. Despite different compression
-- formats all the files have header + payload. This is modeled by the template method.
-- Alternating part is the header format, compression algorithm and the checksum calculation.
-- 

--
-- Template for all compression is to join header and payload together. Header creator may use
-- compressed data length and checksum. Compression uses only the original data.
-- 

type HeaderFunction = Int -> Int -> [Char]
type CompressFunction = [Char] -> [Char]
type ChecksumFunction = [Char] -> Int

zipTemplateMethod :: HeaderFunction -> CompressFunction -> ChecksumFunction -> [Char] -> [Char]
zipTemplateMethod headerCreator compressAlgorithm checksumAlgorithm input = do
    let payload = compressAlgorithm input
    let size = length payload
    let checksum = checksumAlgorithm input
    let header = headerCreator size checksum

    header ++ payload

--
-- Simple header creator joins size and checksum
-- 
simpleHeader :: HeaderFunction
simpleHeader size checksum =
    show size ++ show checksum

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
    let simpleZip = zipTemplateMethod simpleHeader simpleCompress simpleChecksum
    let compressed =  simpleZip phrase

    putStrLn compressed
