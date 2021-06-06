module BinarySearchTree

type Node =
    | Branch of int * Node list
    | Leaf of int

let data node =
    match node with
    | Branch (data, _) -> data
    | Leaf data -> data


let left node = 
    match node with
    | Leaf _ -> None
    | Branch (value, children) -> 
        children
        |> List.tryFind(fun n -> (data n) < value)

let right node = 
    match node with
    | Leaf _ -> None
    | Branch (value, children) -> 
        children
        |> List.tryFind(fun n -> (data n) > value)

let rec addItem node item =
    match node with
    | Some n -> 
        match n with
        | Leaf value -> Branch (value, [Leaf item])
        | Branch (value, children) ->
            let candidate = if item <= value then left n else right n
            match candidate with
            | Some child -> addItem Some child item
            | None -> { Branch }
    | None -> Leaf item
    |> Some
    

type Node2 =
    {
        Data: int;
        Left: Node option;
        Right: Node option;
    }

let left2 node  = node.Left

let right2 node = node.Right



let rec addItem2 node item =
    match node with
    | Some n -> 
        if item <= n.Data then 
            { n with Left = addItem n.Left item }
        else
            { n with Right = addItem n.Right item }
    | None -> { Data=item; Left=None; Right=None }
    |> Some
    
let rec create items =
    items 
    |> List.fold addItem None
    |> Option.defaultWith(fun _ -> failwith "We should never be here")

let sortedData node =
    let rec sortedNodeData optNode =
        match optNode with
        | None -> []
        | Some n -> sortedNodeData n.Left @ (n.Data :: sortedNodeData n.Right)
    sortedNodeData (Some node)