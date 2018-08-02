///
/// ADAPTER
/// 
/// Convert the interface of a class into another interface clients expect. Adapter lets classes
/// work together that couldn't otherwise because of incompatible interfaces.
/// 

///
/// CONCLUSION
///
/// In functional world there is only two things to adapt: data and functions. Since the two things
/// are separated also the adaptation functionality is fine-graned.
///
/// Function adaptation is converting a function signature to another
/// Data adaptation is converting data from one form to another
///
/// Functional data is always immutable. Thus converting between different formats is done simply
/// with functions. There is no danger that data gets corrupted.
///
/// old : divideOld (numerator : float) (denominator : float) : (bool * float)
/// new : divideNew (numerator : float) (denominator : float) : float option
///
/// let divideNew numerator denominator =
///   match divideOld numerator denominator with
///     | true, value -> Some(value)
///     | false, _ -> None
///
///

///
/// Example
/// 
/// This example demonstrates how an old interface (Adaptee module) is adapted to a new interface
/// (Adapter module). It includes both forms of adaption - data and function - adaption.
/// 

///
/// Module to be adapted to have a new interface
/// 
module Adaptee =
    type Circle = { Diameter : int; Position : int * int }
    let moveHorizontal delta circle =
        let { Diameter = diameter; Position = (x, y) } = circle
        { Diameter = diameter; Position = (x + delta, y)}
    let moveVertical delta circle =
        let { Diameter = diameter; Position = (x, y) } = circle
        { Diameter = diameter; Position = (x, y + delta)}

    let scale percentage circle =
        if percentage < 0
            // negative scaling not allowed
        then (false, circle)
        else (true, { circle with Diameter = circle.Diameter * percentage / 100 })

///
/// Module to adapt old Adaptee interface to new interface
/// 
module Adapter =
    type Circle = { Radius : int; Position : int * int }

    ///
    /// Data adaption (adaptee format to current format and vice versa)
    /// 
    let private toObsolete circle =
        let { Radius = radius; Position = position } = circle
        { Adaptee.Diameter = radius * 2; Adaptee.Position = position }

    let private fromObsolete circle =
        let { Adaptee.Diameter = diameter; Adaptee.Position = position } = circle
        { Radius = diameter / 2; Position = position }


    ///
    /// Function adaption (different inputs)
    /// 
    let move deltaX deltaY circle =
        circle
        |> toObsolete
        |> Adaptee.moveHorizontal deltaX
        |> Adaptee.moveVertical deltaY
        |> fromObsolete

    ///
    /// Function adaption (different output)
    /// 
    let scale percentage circle =
        let (success, circle) = circle |> toObsolete |> Adaptee.scale percentage

        if success
        then Some (fromObsolete circle)
        else None

let test() =
    let original = { Adapter.Radius = 10; Adapter.Position = (0, 0) }

    printfn "created %A" original
    let modified = original |> Adapter.move 10 8 |> Adapter.scale 200
    printfn "result %A" modified

test()