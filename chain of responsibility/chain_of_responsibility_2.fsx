///
/// CHAIN OF RESPONSIBILITY
/// 
/// Avoid coupling the sender of a request to its receiver by giving more than one object a chance
/// to handle the request. Chain the receiving objects and pass the request along the chain until an
/// object handles it
/// 



let isOdd a =
    a % 2 = 0

let isEven a = 
    a % 2 = 1

let isSquare a =
    let b = int ((float a) ** (1.0 / 2.0) + 0.5)
    b * b = a

let isCubic a = 
    let b = int ((float a) ** (1.0 / 3.0) + 0.5)
    b * b * b = a



