///
/// ABSTRACT FACTORY
/// 
/// Provide an interface for creating families of related or dependent objects without specifying
/// their concrete classes.
/// 

///
/// CONCLUSION
/// 
/// In functional programming related or dependent values are created with discriminated unions.
/// Related functions working on the specific types has to be provided also. This enables 3rd party
/// modules to be attached to the program easily.
/// 
/// If new values under the type system has to be added or removed, the concequences are similar as
/// in the object-oriented version. That is, the existing type hierarchies must be refactored to
/// match the new hierarchies.
/// 
/// Lack of function polymorphism in F# makes it a bit trickier to provide the functions working.
/// with the factory created types. However, these problems can be overcome with statically
/// resolved type parameters. Another way would be to partially apply the framework functions with
/// the specific module functions. In Haskell typeclasses can be used instead.

///
/// NOTE 
/// 
/// Instead of creating separate type hierarchies (Window, ScrollBar in the original GoF exsample)
/// it is easier to just create discriminated unions with all the related or dependant values. It
/// is also possible to create separate typehierarchies, but this way feels less functional and
/// more tedious.
///

///
/// NOTE
/// 
/// Instead of using a factory record type, plain functions could be applied to the framework
/// function directly.
/// 

///
/// Example
/// 
/// In this example some related data is provided by two separate systems: namely Steam and
/// Electric systems. Framework functions work on these data types, by using the functions provided
/// by the corresponding modules.
/// 

module Steam =
    type System = Boiler | Pipe | Engine of int

    type System with
        static member Label (part : System) : string =
            match part with
                | Boiler -> "boiler"
                | Pipe -> "-pipe-"
                | Engine power -> sprintf "engine (%A kw)" power
    let boilerCreator() = Boiler
    let pipeCreator() = Pipe
    let engineCreator power = Engine power

module Electric =
    type System = Generator | Wire | Motor of int

    let generatorCreator() = Generator
    let wireCreator() = Wire
    let motorCreator power = Motor power

    type System with
        static member Label (part : System) : string =
            match part with
                | Generator -> "generator"
                | Wire -> "-wire-"
                | Motor power -> sprintf "motor (%A kw)" power

module Framework =
    type Factory<'T> = {
        CreateProvider : unit -> 'T
        CreateConnector : unit -> 'T
        CreateConsumer : int -> 'T
    }
    let systemCreatorTemplate factory length power =
        let providerCreator = factory.CreateProvider
        let connectorCreator = factory.CreateConnector
        let consumerCreator = factory.CreateConsumer
        [providerCreator()] @ List.init length (fun _ -> connectorCreator()) @ [consumerCreator power]

    let inline printSystem (system : ^t list) : string =
        system
        |> List.map (fun part -> (^t : (static member Label : ^t -> string) part))
        |> List.fold (+) ""


let test() =
    let steamFactory = {
        Framework.Factory.CreateProvider = Steam.boilerCreator
        Framework.Factory.CreateConnector = Steam.pipeCreator
        Framework.Factory.CreateConsumer = Steam.engineCreator
    }
    let electricFactory = {
        Framework.Factory.CreateProvider = Electric.generatorCreator
        Framework.Factory.CreateConnector = Electric.wireCreator
        Framework.Factory.CreateConsumer = Electric.motorCreator
    }

    let steamSystemCreator = Framework.systemCreatorTemplate steamFactory
    let electricSystemCreator = Framework.systemCreatorTemplate  electricFactory

    let steamSystem = steamSystemCreator 2 10
    let electricSystem = electricSystemCreator 1 100

    printfn "steam system: %A" (Framework.printSystem steamSystem)
    printfn "electric system: %A" (Framework.printSystem electricSystem)

test()