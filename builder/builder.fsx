///
/// BUILDER
/// 
/// Separate the construction of a complex object from its representation so that the same
/// construction process can create different representations. 
/// 

///
/// Director : Director module
/// 
/// ConcreteBuilder A : Json module
/// ConcreteBuilder B : Simple module
/// 

//=================================================================================================
// Json module (ConcreteBuilder A) - Starts
//=================================================================================================

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

//=================================================================================================
// Json module (ConcreteBuilder A) - Ends
//=================================================================================================


//=================================================================================================
// Simple module (ConcreteBuilder B) - Stars
//=================================================================================================

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

//=================================================================================================
// Simple module (ConcreteBuilder B) - Ends
//=================================================================================================


//=================================================================================================
// Director module - Starts
//=================================================================================================

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

//=================================================================================================
// Director module - Ends
//=================================================================================================


//=================================================================================================
// Example - Starts
//=================================================================================================

let bookInformation = {
    Director.Author = { Lastname = "Malli"; Firstname = "Milla" }
    Director.CoAuthors = None
    Director.PageCount = 213
    Director.Isbn = "ABC-DEFG-123"
}

let jsonBuilder = Director.parseBookInformation Json.startDocument Json.endDocument Json.addAthor Json.addPageCount
bookInformation |> jsonBuilder |> Json.stringify

let simpleBuilder = Director.parseBookInformation Simple.startDocument Simple.endDocument Simple.addAuthor Simple.addPageCount
bookInformation |> simpleBuilder |> Simple.stringify

//=================================================================================================
// Example - Ends
//=================================================================================================


