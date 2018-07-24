module LazyMap = 
    type Tree = Node of N | Leaf of L
    and L = { Key : int; Value : int }
    and N = { Key : int; Value : int; Left : Lazy<Tree>; Right : Lazy<Tree> }

    let rec createTree func key delta =
        printfn "creating lazy tree key=%A delta=%A" key delta
        if delta = 0
        then Leaf { Key = key; Value = func key }
        else Node { Key = key
                    Value = func key
                    Left = lazy(createTree func (key - delta) (delta / 2))
                    Right = lazy(createTree func (key + delta) (delta / 2)) }

    let rec iter f tree =
        match tree with
        | Leaf(leaf) ->
            f leaf.Value
        | Node(node) ->
            iter f (node.Left.Force())
            f node.Value
            iter f (node.Right.Force())

    let rec find key tree =
        match tree with
        | Leaf(leaf) when leaf.Key = key ->
            Some(leaf.Value)
        | Node(node) when node.Key = key ->
            Some(node.Value)
        | Node(node) when node.Key < key ->
            find key (node.Left.Force())
        | Node(node) when node.Key > key ->
            find key (node.Right.Force())
        | _ ->
            None
        


    createTree (( ~- )) 100 2
    |> iter (printfn "%A")