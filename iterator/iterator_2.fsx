///
/// ITERATOR
/// 
/// Provide a way to access the elements of an aggregate object sequentially without exposing its
/// underlying representation.
/// 

///
/// Example 1
/// 
/// List iterator to traverse through list
/// 

type Stack<'a> = Stack of 'a list


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


    ([1 .. 10 ], []) |> forward |> forward |> forward |> backward


module QueueIterator =
    ///
    ///  Queue is implemented as doubly linked list
    /// 
    
    type Queue = { First : Node; Last : Node }
    and Node = Empty | Node of int * Node * Node


    type Thing = { Head : }

    ///
    /// Basic queue operations
    /// 
    let enqueue a queue =
        match queue with
            | { First = Empty; Last = Empty } ->
                let node = Node (a, Empty, Empty)
                { First = node; Last = node }
            | { First = _; Last = Node (a, _, } -> 
                { queue with Last =}


    let dequeue
