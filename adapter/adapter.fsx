///
/// ADAPTER
/// 
/// Convert the interface of a class into another interface clients expect. Adapter lets classes
/// work together that couldn't otherwise because of incompatible interfaces.
/// 

///
/// NOTES
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


///================================================================================================
/// Complex Example - Begin
///================================================================================================

// Definitions in the old interface
module V1 =
    type Product = { id : string; description : string; priceTag : string }

    let  parseFloat str =
        match System.Double.TryParse str with
            | true, f -> Some(f)
            | _ -> None

    let parsePrice (priceTag : string) =
        match priceTag.Split ' ' |> List.ofArray with
            | price::_::[] -> parseFloat(price)
            | _ -> None

    let costOf count product =
        match parsePrice(product.priceTag) with
            | Some(price) -> Some(float count * price)
            | _ -> None


// Definitions in the new interface
module V2 =
    type Currency = EUR | USD | GBP

    type Product = { id : int; description : string; price : float; currency : Currency }

    let convertToEur currency price =
        let factor = match currency with EUR -> 1.00 | USD -> 1.0 / 1.17 | GBP -> 1.0 / 0.88

        factor * price

    let convertFromEur currency price =
        1.0 / (convertToEur currency price)

    let costOfBasket products currency : float =
        products
        |> Seq.map (fun (count, product) -> count, convertToEur currency product.price)
        |> Seq.sumBy (fun (count, price) -> (float count) * price)

// Functions to adapt products and functionalities between old and new formats
module ProductAdapter = 

    //
    // Adapts product from old format to new format and with error checking
    //
    let upgradeProduct (oldProduct : V1.Product) : V2.Product option = 
        let parseFloat str =
            match System.Double.TryParse str with
                | true, f -> Some(f)
                | _ -> None
        
        let parseInt str =
            match System.Int32.TryParse str with
                |true, i -> Some(i) | _ -> None
        
        let parseCurrency str =
            match str with 
                | "€" -> Some(V2.EUR)
                | "$" -> Some(V2.USD)
                | "£" -> Some(V2.GBP)
                | _ -> None

        let parsePriceTag (str : string) =
            match str.Split ' ' |> List.ofArray with
                | price::currency::[] -> parseFloat(price), parseCurrency(currency)
                | _ -> None, None


        let id = parseInt oldProduct.id
        let description = oldProduct.description
        let price, currency = parsePriceTag oldProduct.priceTag

        match id, price, currency with
            | Some(id), Some(price), Some(currency) ->
                Some({ id = id; description = description; price = price; currency = currency})
            | _ -> None

    ///
    /// Adapts product from new format to old format with error checking
    /// 
    let downgradeProduct (newProduct : V2.Product) : V1.Product option =
        let convertCurrency currency =
            match currency with
                | V2.EUR -> "€"
                | V2.USD -> "$"
                | V2.GBP -> "£"

        let id = newProduct.id.ToString()
        let description = newProduct.description
        let price = newProduct.price.ToString()
        let currency = convertCurrency newProduct.currency

        Some({ id = id; description = description; priceTag = price + " " + currency })

//=================================================================================================
// Complex Example - End
//=================================================================================================

let oldProduct : V1.Product = { id = "200"; description = "banana"; priceTag = "1.99 €" }

V1.costOf 10 oldProduct
