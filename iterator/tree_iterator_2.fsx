
type Tree<'a> = Empty | Node of 'a * Tree<'a> * Tree<'a>
type Crumb<'a> = Left of Tree<'a> | Right of Tree<'a>

let left (subtree, crumbs) =
    match subtree with
        | Node (_, Empty, _) ->
            (subtree, crumbs)
        | Node (a, l, r) -> 
            let crumb = Right (Node (a, Empty, r))
            (l, crumb::crumbs)

let right (subtree, crumbs) =
    match subtree with
        | Node (_, _, Empty) ->
            (subtree, crumbs)
        | Node (a, l, r) -> 
            let crumb = Left (Node (a, l, Empty))
            (r, crumb::crumbs)

let up (subtree, crumbs) =
    match crumbs with
        | [] ->
            (subtree, crumbs)
        | (Left (Node (a, l, _)))::crumbs ->
            (Node (a, l, subtree), crumbs)
        | (Right (Node (a, _, r)))::crumbs ->
            (Node (a, subtree, r), crumbs)

let get (subtree, _) =
    match subtree with | Node (a, _, _) -> a



// let tree = Node (3, Node (1, Node (0, Empty, Empty), Node (2, Empty, Empty)),
//             Node (5, Node (4, Empty, Empty), Node (6, Empty, Empty)))
let tree = Node (3, Node (1, Node (0, Empty, Empty), Empty),
            Node (5, Node (4, Empty, Empty), Node (6, Empty, Empty)))


let test = (tree, []) |> right |> right |> up |> up
(fst test) = tree


let rec first (subtree, crumbs) =
    match subtree with
        | Node (_, Empty, _) ->
            (subtree, crumbs)
        | Node (a, l, r) ->
            let crumb = Right (Node (a, Empty, r))
            first (l, crumb::crumbs)

let rec until predicate f a =
    if predicate a
    then until predicate f (f a)
    else a

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


// Rule 1 : If has right child => right
// Rule 2 : If no right child -> up until R popped


(tree, []) |> first |> get
(tree, []) |> first |> forward |> get
(tree, []) |> first |> forward |> forward |> get
(tree, []) |> first |> forward |> forward |> forward |> get
(tree, []) |> first |> forward |> forward |> forward |> forward |> get
(tree, []) |> first |> forward |> forward |> forward |> forward |> forward |> get
(tree, []) |> first |> forward |> forward |> forward |> forward |> forward |> forward |> get
(tree, []) |> first |> forward |> forward |> forward |> forward |> forward |> forward |> forward |> get
