module SimpleLinkedList

type Element<'a> =
    {
        Value: 'a
        Next: Node<'a>
    }
and Node<'a> =
| Null
| Node of Element<'a>

let nil = Null

let create x n = 
    Node {Value = x; Next = n}

let isNil x = 
    x = Null

let next x =
    match x with
    | Null -> Null
    | Node n -> n.Next

let datum x =
    match x with
    | Null -> failwith "No value"
    | Node n -> n.Value

let toList x =
    let rec toList' node =
        match node with
        | Null -> []
        | Node n -> n.Value :: toList' n.Next
    toList' x

let fromList xs =
    let rec fromList' list next =
        match list with
        | [] -> next
        | head::tail -> 
            let node = Node { Value=head; Next=next} 
            fromList' tail node
    fromList' (xs |> List.rev) Null

let reverse x =
    let rec reverse' source next =
        match source with
        | Null -> next
        | Node n -> reverse' n.Next (Node {Value= n.Value; Next=next})
    reverse' x Null