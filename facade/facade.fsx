///
/// FACADE
/// 
/// Provide a unified interface to a set of interfaces in a subsystem. Facade defines a
/// higher-level interface that makes the subsystem easier to use.
/// 

///
/// Applicativity
///
/// Use the Facade pattern when
/// - you want to provide a simple interface to a complex subsystem. Subsystems often get more
///   complex as they evolve. Most patterns, when applied, result in more and smaller classes. This
///   makes the subsystem more reusable and easier to customize, but it also becomes harder to use
///   for clients that don't need to customize it. A facade can provide a simple default view of
///   the subsystem that is good enough for most clients. Only clients needing more customizability
///   will need to look beyond the facade.
/// - there are many dependencies between clients and the implementation classes of an abstraction.
///   Introduce a facade to decouple the subsystem from clients and other subsystems, thereby
///   promoting subsystem independence and portability.
/// - you want to layer your subsystems. Use a facade to define an entry point to each subsystem
///   level. If subsystems are dependent, then you can simplify the dependencies between them by
///   making them communicate with each other solely through their facades.
/// 

///
/// REMARKS
/// 
/// Seems that Facade design pattern is usable also in the functional programming. It is very
/// natural in functional programming to compose multiple functions simpler functions to one
/// more complext function. This is exactly how the Facade works in the functional programming.
/// 

///
/// Example
/// 
/// In this example Compression module contains some low level functions to create compressed data.
/// However, using the module directly is bit tricky. Zipper module is the Facade for the
/// Compression module, offering simple interface to do most frequent data compression.
/// 

///
/// Complex module offering variety of headers, compression algorithms and checksums
/// 
module Compression = 

    let losslessAlgorithmId = [0x01uy; 0x02uy]
    let lossyAlgorithmId = [0x11uy; 0x12uy]

    ///
    /// Some header formats
    /// 
    let simpleHeader size algorithm =
        [0x01uy] @ size @ algorithm

    let complexHeader size packingAlgorithm checksumAlgorithm =
        [0x02uy] @ size @ packingAlgorithm @ checksumAlgorithm



    ///
    /// Some compression algorithms
    /// 
    let lossless data : byte list = data
    let lossy data : byte list =
        ignore data
        []

    ///
    /// Some checksum algorithms
    /// 
    let simple data =
        data |> List.length |> fun v -> [byte v]

    let weird data =
        data |> List.filter (fun b -> b = 0xAFuy) |> List.length |> fun v -> [byte v]

    ///
    /// Some utility functions
    /// 
    let bytes (i : int) : byte list =
        [byte (i >>> 24); byte (i >>> 16); byte (i >>> 8); byte (i >>> 0) ]




///
/// Facade module offering simple interface to do the most often used things with the complex
/// compression module
/// 
module Zipper =

    ///
    /// Offer the most used compression method cleanly in one function
    /// 
    let zip data =
        let content = data |> Compression.lossless
        let size = content |> List.length |> Compression.bytes
        let algorithm = Compression.losslessAlgorithmId

        let header =  Compression.simpleHeader size algorithm
        
        header @ content


let test() =
    let data = "a brown fox jumped over the lazy dog"B |> List.ofArray

    let packet = Zipper.zip data

    printfn "%A" packet

test()