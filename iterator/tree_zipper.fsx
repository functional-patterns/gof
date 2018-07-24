
type Tree<'a> = Empty | Node of N<'a>
and N<'a> = { Value : 'a; Left : Tree<'a>; Right : Tree<'a> }

type Crumb<'a> = LeftCrumb of Tree<'a> | RightCrumb of Tree<'a>


let freeTree = Node {
    Value = 5;
    Left = Node {
        Value = 3;
        Left = Node {
            Value = 1;
            Left = Empty;
            Right = Empty;
        }
        Right = Node {
            Value = 2;
            Left = Empty;
            Right = Empty;
        }
    }

    Right = Node {
        Value = 10;
        Left = Node {
            Value = 7;
            Left = Empty;
            Right = Empty;
        }
        Right = Node {
            Value = 15;
            Left = Empty;
            Right = Empty;
        }
    }
}

let left (subtree : Tree<'a>, crumbs : Crumb<'a> list) : (Tree<'a> * Crumb<'a> list) =
    match subtree with
        | Node { Value = _; Left = Empty; Right = _ }
            -> (subtree, crumbs)
        | Node { Value = v; Left = left; Right = right }
            -> (left, RightCrumb (Node { Value = v; Left = Empty; Right = right }) :: crumbs )


let right (subtree : Tree<'a>, crumbs : Crumb<'a> list) : (Tree<'a> * Crumb<'a> list) =
    match subtree with
        | Node { Value = _; Left = _; Right = Empty }
            -> (subtree, crumbs)
        | Node { Value = v; Left = left; Right = right }
            -> (right, LeftCrumb (Node { Value = v; Left = left; Right = Empty }) :: crumbs )

let up (subtree : Tree<'a>, crumbs : Crumb<'a> list) : (Tree<'a> * Crumb<'a> list) =
    match crumbs with
        | RightCrumb (Node c)::cs ->
            (Node { c with Left = subtree}, cs)
        | LeftCrumb (Node c)::cs ->
            (Node { c with Right = subtree}, cs)

let modify a (subtree : Tree<'a>, crumbs : Crumb<'a> list) : (Tree<'a> * Crumb<'a> list)  =
    match subtree with
        | Empty -> (subtree, crumbs)
        | Node n -> (Node { n with Value = a }, crumbs)



(freeTree, []) |> left |> left |> up |> up
(freeTree, []) |> right |> right |> modify 20 |> up |> up
(freeTree, []) |> modify 20

