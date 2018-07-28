///
/// STRATEGY
/// 
/// Define a family of algorithms, encapsulate each one, and make them interchangeable. Strategy
/// lets the algorithm vary independently from clients that use it. 
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
/// Example
/// 
/// In this example different strategies are implemented and used to divide a phrase to words.
/// 


let phrase = "a brown fox jumped over the lazy dog" |> Seq.map char |> Seq.toList

type CompositionStrategy = char list -> char list list

///
/// Safe versions of the default skip and take functions
/// 
let take count items = List.truncate count items
let skip count items = if List.length items < count then [] else List.skip count items


let fixedStrategy length text =
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
        



phrase |> Seq.map char |> Seq.toList

fixedStrategy 10 phrase
simpleStrategy phrase


phrase |> List.takeWhile (fun c -> c <> ' ')


phrase |> Seq.truncate 10 |> List.ofSeq




"foobar" |> Seq.map char |> Seq.toList |> List.tryFindIndex (fun c -> c = 'b')
