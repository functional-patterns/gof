///
/// CHAIN OF RESPONSIBILITY
/// 
/// Avoid coupling the sender of a request to its receiver by giving more than one object a chance
/// to handle the request. Chain the receiving objects and pass the request along the chain until an
/// object handles it
/// 

///
/// CONCLUSION
/// 
/// In functional programming functions can be chained easily. Monad is used to add some result
/// value, which then may be accumulated by the functions in the chain.
/// 
/// Like in the object-oriented counter part, the functions need to have same signature to make
/// the chaining process easy.
/// 

///
/// COMPARISON
/// 
/// In object-oriented version the handlers are object. In functional programming the handlers are
/// functions. Since pure functional programming cannot have side effects, the Chain of
/// Responsibility cannot alter any state or do I/O operations. Thus in functional version the
/// functionality is limited to alter the output.
/// 

///
/// Example
/// 
/// In this example a integer is tested with various methods. It is passed through a chain of
/// functions which add log messages according to the success of the test.
/// 

///
/// Some test methods
/// 
let isOdd a =
    a % 2 = 1

let isEven a = 
    a % 2 = 0

let isSquare a =
    let b = int ((float a) ** (1.0 / 2.0) + 0.5)
    b * b = a

let isCubic a = 
    let b = int ((float a) ** (1.0 / 3.0) + 0.5)
    b * b * b = a

///
/// Function to return log message of the given test instead of plain boolean value.
/// 
let lift log testFunction testValue =
    if testFunction testValue
    then [log]
    else []
    
///
/// Lifted test functions (returning a log message)
/// 
let isOddLog = lift "odd" isOdd
let isEvenLog = lift "even" isEven
let isSquareLog = lift "square" isSquare
let isCubicLog = lift "cubic" isCubic

///
/// Simple function to star monad chain
/// 
let pure a : (int * string list) = a, []

///
/// Bind operator for the chain
/// 
let (>>=) mA f =
    let a = fst mA
    let logs = snd mA

    let log = f a

    (a, (log @ logs))

let test() =
    let chainOfResponsibility a =
        pure a >>= isOddLog >>= isEvenLog >>= isSquareLog >>= isCubicLog
    printfn "%A" (chainOfResponsibility 64)
    printfn "%A" (chainOfResponsibility 15)

test()