///
/// STRATEGY
/// 
/// Define a family of algorithms, encapsulate each one, and make them interchangeable. Strategy
/// lets the algorithm vary independently from clients that use it. 
/// 

///
/// CONCLUSION
/// 
/// In functional programming strategy is just a family of functions having the same signature and
/// conceptually performing the same kind of task (like compressing data or sorting items).
/// 

///
/// NOTES
/// 
/// Remember to compare different design patterns in functional programming. Many of them -
/// including Strategy - are just function composition and partial application. Check if the design
/// patterns having similar class diagrams in object-oriented programming have similar structures
/// in functional programming.
/// 

///
/// EXAMPLE
/// 
/// In this example different strategies are implemented and used to divide a phrase to words.
/// 


// Safe versions of the default skip and take functions
let take count items = List.truncate count items
let skip count items = if List.length items < count then [] else List.skip count items


// Some strategies to chop sentences to lines
let fixedStrategyTemplate length text =
    let rec chop text lines =
        match text with
            | [] ->
                lines
            | _ ->
                let line = text |> take length
                let remaining = text |> skip length
                chop remaining (lines @ [line])
    chop text []

let simpleStrategy text =
    let rec chop text lines =
        match text with
            | [] ->
                lines
            | _ ->
                let text = List.skipWhile (fun c -> c = ' ') text
                let line = List.takeWhile (fun c -> c <> ' ') text
                let count = List.length line
                let remaining = skip count text
                chop remaining (lines @ [line])

    chop text []

let test() =
    let phrase = "a brown fox jumped over the lazy dog" |> Seq.map char |> Seq.toList

    // Use of partial application to make the template to have same signature as other strategies
    let fixedStrategy = fixedStrategyTemplate 10

    let fix = fixedStrategyTemplate 10 phrase
    let simple = simpleStrategy phrase

    printfn "fixed: %A" fix
    printfn "simple: %A" simple

test()
