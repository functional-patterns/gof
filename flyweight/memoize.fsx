
let memoize fn =
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  
  fun key ->
    match cache.TryGetValue key with
    | true, value ->
        value
    | false, _ ->
        let value = fn (key)
        cache.Add(key, value)
        value