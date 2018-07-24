///
/// TEMPLATE METHOD
/// 
/// Define the skeleton of an algorithm in an operation, deferring some steps to subclasses.
/// Template Method lets subclasses redefine certain steps of an algorithm without changing the
/// algorithm's structure.
/// 

///
/// CONCLUSION
/// 
/// Template Method is one of the simplest object-oriented design patterns to be implemented in
/// pure functional way. Essentially it is just a simple function composition.
/// 
/// Compared to object-oriented version the functional version is way shorter.
/// 

///
/// Example
///
/// This example defines a template method for compressing files. Despite different compression
/// formats all the files have header + payload + checksum. This is modeled by the template method.
/// Alternating part is the header format, compression algorithm and the checksum calculation.
/// 

///
/// Template for all compression is to join header and payload together. Header creator may use
/// compressed data length and checksum. Compression uses only the original data.
/// 
let zipTemplateMethod headerCreator compressAlgorithm checksumAlgorithm data =
    let payload, size = compressAlgorithm data
    let checksum = checksumAlgorithm data
    let header = headerCreator size checksum

    header @ payload

///
/// Simple header creator joins size and checksum
/// 
let simpleHeaderCreator size checksum =
    [size] @ [checksum]

///
/// Simple compress algorithm does nothing
/// 
let simpleCompressAlgorithm data =
    data, byte (List.length data)

///
/// Simple checksum is calculated by xorring all the bytes in the data
/// 
let simpleChecksumAlgorithm data =
    let bitwiseXor = ( ^^^ )
    data |> List.reduce bitwiseXor

///
/// Partial application is used to the template method to create a concrete compress function
/// 
let simpleZip = zipTemplateMethod simpleHeaderCreator simpleCompressAlgorithm simpleChecksumAlgorithm


let test() =
    "a brown fox jumped over the lazy dog"B
    |> List.ofArray
    |> simpleZip

test()