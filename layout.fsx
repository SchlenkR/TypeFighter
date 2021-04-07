type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let example_tree =
    Node('a',
        Node('b',
            Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))

let rec count_leaves tree =
    match tree with
        | Empty -> 0
        | Node(_,Empty,Empty) -> 1
        | Node(_,a,b) -> (count_leaves a) + (count_leaves  b)

type pos_binary_tree<'a> =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree 
    (*N(w,x,y,l,r) represents a (non-empty) binary tree with root w "positioned" at (x,y), and subtrees l and r *)

let make_pos_binary_tree  (tree:binary_tree<_>) =
    let rec layout t x depth =
        match t with
            | Empty -> (x,E)
            | Node(a,l,r) ->
                let x',lTree = layout l x (depth+1)
                let x'',rTree = layout r (x'+1) (depth+1)
                (x'',N(a,x',depth,lTree,rTree))
    let finalX,finalTree = layout tree 1 1
    finalTree
// [/snippet]

// [snippet:Example and rendering]
let laidOutTree = make_pos_binary_tree example_tree

let canvas = Array2D.init 50 50 (fun _ _ -> ' ')

let rec draw tree =
    match tree with
        | E -> ()
        | N(v,x,y,l,r) ->
            canvas.[y,x] <- v
            draw l
            draw r

draw laidOutTree

let res =
    System.String
        [|
            for y in 0..49 do
                for x in 0..49 do
                    yield canvas.[y,x]
                yield '\n'
        |]
    
