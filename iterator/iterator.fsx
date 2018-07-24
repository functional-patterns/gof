///
/// ITERATOR
/// 
/// Provide a way to access the elements of an aggregate object sequentially without exposing its
/// underlying representation.
/// 

///
/// Example 1
/// 
/// List iterator to travel through lists and tree iterator to travel through trees
/// 


module ListIterator =
    let forward (stack, visited) =
        match stack with
            | [] ->
                (stack, visited)
            | head::tail ->
                (tail, head::visited)

    let backward (stack, visited) =
        match visited with
            | [] ->
                (stack, visited)
            | head::tail ->
                (head::stack, tail)

    let rec first (stack, visited) =
        match visited with
            | [] ->
                (stack, visited)
            | head::tail ->
                first (head::stack, tail)

    let rec last (stack, visited) =
        match stack with
            | [] ->
                (stack, visited)
            | head::tail ->
                first (tail, head::visited)

    let get (stack, _) =
        let a::_ = stack
        a


    ([1 .. 10 ], []) |> forward |> forward |> forward |> backward


module TreeIterator =
    type Tree<'a> = Empty | Node of 'a * Tree<'a> * Tree<'a>
    type Crumb<'a> = Left of Tree<'a> | Right of Tree<'a>

    ///
    /// Function to select the left branch of the tree
    /// 
    let left (subtree, crumbs) =
        match subtree with
            | Node (_, Empty, _) ->
                (subtree, crumbs)
            | Node (a, l, r) -> 
                let crumb = Right (Node (a, Empty, r))
                (l, crumb::crumbs)

    ///
    /// Function to select the right branch of the tree
    /// 
    let right (subtree, crumbs) =
        match subtree with
            | Node (_, _, Empty) ->
                (subtree, crumbs)
            | Node (a, l, r) -> 
                let crumb = Left (Node (a, l, Empty))
                (r, crumb::crumbs)

    ///
    /// Function to select parent node of the tree
    /// 
    let up (subtree, crumbs) =
        match crumbs with
            | [] ->
                (subtree, crumbs)
            | (Left (Node (a, l, _)))::crumbs ->
                (Node (a, l, subtree), crumbs)
            | (Right (Node (a, _, r)))::crumbs ->
                (Node (a, subtree, r), crumbs)

    ///
    /// Function to get current element
    /// 
    let get (subtree, _) =
        match subtree with | Node (a, _, _) -> a



    ///
    /// Function to find the first node in the tree
    /// 
    let rec first (subtree, crumbs) =
        match subtree with
            | Empty ->
                (subtree, crumbs)
            | Node (_, Empty, _) ->
                (subtree, crumbs)
            | Node (a, l, r) ->
                let crumb = Right (Node (a, Empty, r))
                first (l, crumb::crumbs)

    ///
    /// Function to find the last node in the tree
    /// 
    let rec last (subtree, crumbs) =
        match subtree with
            | Empty ->
                (subtree, crumbs)
            | Node (_, _, Empty) ->
                (subtree, crumbs)
            | Node (a, l, r) ->
                let crumb = Left (Node (a, l, Empty))
                last (r, crumb::crumbs)

    ///
    /// Helper function to repeat function f until predicate is true
    /// 
    let rec until predicate f a =
        if predicate a
        then until predicate f (f a)
        else a

    ///
    /// Function to traverse forward
    /// 
    let forward (subtree, crumbs) =
        match subtree with
            | Empty ->
                (subtree, crumbs)
            | Node (_, _, Empty) ->
                   let predicate (_, crumbs) =
                    match crumbs with
                        | [] -> false
                        | (Right _)::_ -> false
                        | (Left _)::_ -> true
                   up (until predicate up (subtree, crumbs))
            | _ ->
                let right = right (subtree, crumbs)
                first right

    ///
    /// Function to traverse backward
    /// 
    let backward (subtree, crumbs) =
        match subtree with
            | Empty ->
                (subtree, crumbs)
            | Node (_, Empty, _) ->
                   let predicate (_, crumbs) =
                    match crumbs with
                        | [] -> false
                        | (Right _)::_ -> true
                        | (Left _)::_ -> false
                   up (until predicate up (subtree, crumbs))
            | _ ->
                let left = left (subtree, crumbs)
                last left

let test() =
    let leaf0 = TreeIterator.Node (0, TreeIterator.Empty, TreeIterator.Empty)
    let leaf2 = TreeIterator.Node (2, TreeIterator.Empty, TreeIterator.Empty)
    let leaf4 = TreeIterator.Node (4, TreeIterator.Empty, TreeIterator.Empty)
    let leaf6 = TreeIterator.Node (6, TreeIterator.Empty, TreeIterator.Empty)

    let branch1 = TreeIterator.Node (1, leaf0, leaf2)
    let branch5 = TreeIterator.Node (5, leaf4, leaf6)

    let tree = TreeIterator.Node (3, branch1, branch5)

    let secondInTree = (tree, []) |> TreeIterator.first |> TreeIterator.forward |> TreeIterator.get


    let list = [ 0 .. 6]
    let secondInList = (list, []) |> ListIterator.forward |> ListIterator.get

    printfn "2nd item in tree is %A and in list is %A" secondInTree secondInList

test()