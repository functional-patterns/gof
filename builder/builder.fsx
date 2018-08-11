///
/// BUILDER
/// 
/// Separate the construction of a complex object from its representation so that the same
/// construction process can create different representations. 
/// 

///
/// Example
/// 
/// This example demonstrates how common builder can be used to create documents in different
/// formats.
/// 

module Json =

    type JsonToken = string
    type JsonDocument = { tokens : list<JsonToken> }

    let startDocument = { tokens = [] }
    let endDocument document = document

    let addAthor lastname firstname document =
        let tokens = List.append document.tokens [(sprintf "author: \"%s, %s\"" lastname firstname)]
        { document with tokens = tokens }
    
    let addPageCount (pageCount : int) document =
        let tokens = List.append document.tokens [(sprintf "pages: %i" pageCount)]
        { document with tokens = tokens }

    let stringify document =
        "{ " + List.reduce (fun s t -> s + ", " + t) document.tokens + " }"



module Simple =

    type SimpleDocument = { content : string }

    let startDocument = { content = "# Begin of Simple document" }
    let endDocument document = { content = document.content + "\n" + "# End" }

    let addAuthor lastname firstname document =
        { content = document.content + "\n" + "author=" + lastname + ", " + firstname }
   
    let addPageCount pageCount document =
                { content = document.content + "\n" + "pages=" + pageCount.ToString() }

    let stringify document =
        document.content


module Director = 

    type Author = { Lastname : string; Firstname : string }

    type BookInformation = {
        Author : Author
        CoAuthors : list<Author> option
        PageCount : int
        Isbn : string  
    }
    let parseBookInformation startDocument endDocument handleAuthor handlePages bookInformation =
        startDocument
        |> handleAuthor bookInformation.Author.Lastname bookInformation.Author.Firstname
        |> handlePages bookInformation.PageCount
        |> endDocument


let test() =
    let bookInformation = {
        Director.Author = { Lastname = "Malli"; Firstname = "Milla" }
        Director.CoAuthors = None
        Director.PageCount = 213
        Director.Isbn = "ABC-DEFG-123"
    }

    let jsonBuilder = Director.parseBookInformation Json.startDocument
                                                    Json.endDocument
                                                    Json.addAthor
                                                    Json.addPageCount
    let json = bookInformation |> jsonBuilder |> Json.stringify
    
    let simpleBuilder = Director.parseBookInformation Simple.startDocument
                                                      Simple.endDocument
                                                      Simple.addAuthor
                                                      Simple.addPageCount
    let simple = bookInformation |> simpleBuilder |> Simple.stringify

    printfn "json: %A" json
    printfn "simple: %A" simple

test()
