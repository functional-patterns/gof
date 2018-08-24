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
-- This example defines a template function for packing data. The template enforces the steps of the
-- algorithm and the structure of the packet. The altering parts - namely compression and checksum
-- algorithms - can be varied independently.
-- 

type CompressFunction = [Char] -> [Char]
type ChecksumFunction = [Char] -> Int

packTemplate :: CompressFunction -> String -> ChecksumFunction -> String -> [Char] -> [Char]
packTemplate compressionAlgorithm compressionFormat checksumAlgorithm  checksumFormat input = do
    let payload = compressionAlgorithm input
    let checksum = checksumAlgorithm input

    compressionFormat ++ "|" ++ checksumFormat ++ "|" ++ (show checksum) ++ "|" ++ payload

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
    let pack = packTemplate simpleCompress "simple" simpleChecksum "unsecure"
    let packet =  pack phrase

    putStrLn packet
