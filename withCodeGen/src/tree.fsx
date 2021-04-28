#load "./visu/visu.fsx"
open Visu

#load "base.fsx"
open Base

let showUntypedAst (exp: Exp) =
    let getTypeName (l: Lit) =
        match l with
        | LString _ -> "String"
        | LNumber _ -> "Number"
        | LBool _ -> "Bool"
        | LUnit _ -> "Unit"
    
    let getValue (l: Lit) =
        match l with
        | LString x -> x :> obj
        | LNumber x -> x :> obj
        | LBool x -> x :> obj
        | LUnit -> "()" :> obj

    let rec flatten (node: Tree.Node) =
        [
            yield node
            for c in node.children do
                yield! flatten c
        ]
    
    let rec createNodes (exp: Exp) =
        match exp with
        | Lit x ->
            Tree.var $"Lit" $"{getValue x} : {(getTypeName x)}" []
        | Var ident ->
            Tree.var $"Var" ident []
        | App (e1, e2) ->
            Tree.var $"App" "" [ createNodes e1; createNodes e2 ]
        | Abs (ident, body) ->
            Tree.var $"Abs {ident} -> e" "" [ createNodes body ]
        | Let (ident, e, body) ->
            Tree.var $"Let" ident [ createNodes e; createNodes body ]
        | Ext (Prop (name, e)) ->
            Tree.var $"Prop" name [ createNodes e ]
        | Ext (Tup es) ->
            Tree.var $"Tup" "" [ for e in es do createNodes e ]
        | Ext (Record fields) ->
            let labels = fields |> List.map fst |> String.concat ", "
            Tree.var $"Record" labels [ for _,e in fields do createNodes e ]

    createNodes exp |> flatten |> Tree.write

id
^| Let "x" (Num 10.0)
^| Appt (Var "map") [ Prop (Var "c") "BlockEdges"; Abs "be"
^|     Let "id" (Prop (Var "be") "Id")
^|     Record
       [
           "oldId", (Var "id")
           "newId", Appt (Var "add") [ Var "id"; Var "x" ]
       ]]
|> showUntypedAst
