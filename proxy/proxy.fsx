///
/// PROXY
/// 
/// Provide a surrogate or placeholder for another object to control access to it.
/// 

/// Applicability
/// 
/// Proxy is applicable whenever there is a need for a more versatile or sophisticated reference to
/// an object than a simple pointer. Here are several common situations in which the Proxy pattern
/// is applicable:
/// 1. A remote proxy provides a local representative for an object in a different address space.
/// 2. A virtual proxy creates expensive objects on demand. The ImageProxy described in the
///    Motivation is an example of such a proxy.
/// 3. A protection proxy controls access to the original object. Protection proxies are useful
///    when objects should have different access rights. For example, KernelProxies in the
///    Choices operating system [CIRM93] provide protected access to operating system objects.
/// 4. A smart reference is a replacement for a bare pointer that performs additional actions when
///    an object is accessed. Typical uses include
///     - counting the number of references to the real object so that it can be freed
///       automatically when there are no more references (also called smart pointers [Ede92]).
///     - loading a persistent object into memory when it's first referenced.
///     - checking that the real object is locked before it's accessed to ensure that no other
///       object can change it.

///
/// REMARKS
/// 
/// Haskell offers lazy map; which may be used to implement memoization. In this way the proxy
/// can be used to memorize the return values for any given parameters.
/// 
/// Functional programs often feature lazy evaluation. In this sense the possibly expensive
/// computation is deferred later. This is one feature the object oriented counterpart offers.
/// 
/// Design pattern is useful also in data hiding, like it was in object-oriented version.

///
/// Example 1
/// 
/// In this example the core function tests if a number is a prime number. To make it faster,
/// it is wrapped inside a proxy function, which uses a pre-calculated list for the numbers up to
/// 100. For the numbers over 100 the real core function is called.
/// 
/// In expensive calculation a lot of time could be saved by memorizing/pre-calculating the most
/// used values.
/// 

///
/// Function to test if number is a prime number (really inefficient)
/// 
let isPrime (a : int) : bool =
    printfn "isPrime %A" a
    not ([2 .. int (sqrt (float a))]
        |> Seq.exists (fun t -> a % t = 0))

///
/// Function to use pre-calculated proxy values
/// 
let isPrimeProxy (a : int) : bool =
    printfn "isPrimeProxy %A" a
    let primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97];

    if a < 100
    then primes |> List.exists (fun t -> t = a)
    else isPrime a


let test() =
    printfn "is %A prime? - %A"  29 (isPrimeProxy 29)
    printfn "is %A prime? - %A"  99 (isPrimeProxy 99)
    printfn "is %A prime? - %A" 201 (isPrimeProxy 201)
    printfn "is %A prime? - %A" 541 (isPrimeProxy 541)

test()


///
/// Example 2
/// 
/// In this example the core function returns some classified information. The proxy functions
/// control access rights. For example user logged in with high security information is able
/// 

type CriminalRecord = { Name : string; Crime : string }

let criminalRecord ssn =
    let criminals = [ ("JENN", { Name = "Jennifer Lawrence"; Crime = "bad acting" });
                      ("HILL", { Name = "Hillary Clinton"; Crime = "treason"});
                      ("OBAM", { Name = "Barak Obama"; Crime = "treason"});
                      ("COPP", { Name = "David Copperfield"; Crime = "Disappearing"}) ]
                    |> Map.ofList

    criminals |> Map.tryFind ssn

criminalRecord "JENN"

let fbiCriminalRecordProxy ssn =
    criminalRecord ssn

let policeCriminalRecordProxy ssn =
    let hidden = [ "HILL"; "OBAM" ]
    if List.exists (fun t -> t = ssn) hidden
    then None
    else criminalRecord ssn

fbiCriminalRecordProxy "BAD"

let test2() =
    printfn "regular cop searching what Jennifer Larence did"
    printfn "%A" (policeCriminalRecordProxy "JENN")

    printfn "regular cop searching what Hillary Clinton did"
    printfn "%A" (policeCriminalRecordProxy "HILL")

    printfn "FBI guy searching what Hillary Clinton did"
    printfn "%A" (fbiCriminalRecordProxy "HILL")

test2()
