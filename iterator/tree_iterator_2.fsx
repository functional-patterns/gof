
type Tree<'a> = Empty | Node of 'a * Tree<'a> * Tree<'a>
type Crumb<'a> = Left of Tree<'a> | Right of Tree<'a>

let left (subtree, crumbs) =
    match subtree with
        | Node (_, Empty, _) ->
            (subtree, crumbs)
        | Node (a, left, right) -> 
            let crumb = Right (Node (a, Empty, right))
            (left, crumb::crumbs)

let right (subtree, crumbs) =
    match subtree with
        | Node (_, _, Empty) ->
            (subtree, crumbs)
        | Node (a, left, right) -> 
            let crumb = Left (Node (a, left, Empty))
            (right, crumb::crumbs)

let up (subtree, crumbs) =
    match crumbs with
        | [] ->
            (subtree, crumbs)
        | (Left (Node (a, left, _)))::crumbs ->
            (Node (a, left, subtree), crumbs)
        | (Right (Node (a, _, right)))::crumbs ->
            (Node (a, subtree, right), crumbs)



let tree = Node (3, Node (1, Node (0, Empty, Empty), Node (2, Empty, Empty)),
            Node (5, Node (4, Empty, Empty), Node (6, Empty, Empty)))


let test = (tree, []) |> right |> right |> up |> up
(fst test) = tree


let rec first (subtree, crumbs) =
    match subtree with
        | Node (_, Empty, _) ->
            (subtree, crumbs)
        | Node (a, left, right) ->
            let crumb = Right (Node (a, Empty, right))
            first (left, crumb::crumbs)

let rec until predicate f a =
    if predicate a
    then until predicate f (f a)
    else a

let next (subtree, crumbs) =
    match subtree with
        | Empty ->
            (subtree, crumbs)
        | Node (_, _, r) when r <> Empty ->
            let right = right (subtree, crumbs)
            first right
        | Node (_, _, r) when r = Empty ->


// Rule 1 : If has right child => right
// Rule 2 : If no right child -> up until R popped




let rec traverse tree =
    match tree with
        | Empty ->
            ()
        | Node (a, left, right) ->
            traverse left
            printfn "%A" a
            traverse right

traverse tree


let foo = seq [1; 2; 3; 4; 5; 6]



(tree, []) |> left |> right |> up |> up |> right


let rec repeat n f =
    if n = 0
    then id
    elif n = 1
    then f
    else (f >> repeat (n - 1) f)

let rec until predicate f a =
    if predicate a
    then until predicate f (f a)
    else a

