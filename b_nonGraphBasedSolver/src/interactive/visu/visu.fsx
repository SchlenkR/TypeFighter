
(*
    No deps!
*)

open System.IO
open System.Text.Json
open System.Text.Json.Serialization

type JsNode =
    { 
        key: int
        name: string
        varNum: string
        additionalInfo: string
        exprTyp: string
        env: string
        [<JsonPropertyName("fig")>] layout: string
    }

type JsLink =
    { 
        [<JsonPropertyName("from")>] fromNode: int
        [<JsonPropertyName("to")>] toNode: int 
    }

[<AutoOpen>]
module internal Internal =
    module Layouts =
        let tree = "tree"

    module NodeTypes =
        let expr = "Rectangle"

    let writeData (nodesJson: string) (linksJson: string) (layout: string) =
        let json = $"
window.layout = \"{layout}\";
window.nodeDataArray = {nodesJson};
window.linkDataArray = {linksJson};
        "

        let path = Path.Combine(__SOURCE_DIRECTORY__, "data")
        let dataPath = Path.Combine(path, "data.js")
        File.WriteAllText(dataPath, json)

    let serialize (v: obj) =
        JsonSerializer.Serialize(v, JsonSerializerOptions(WriteIndented = true))

module Tree =
    type Node =
        { 
            mutable key: int
            name: string
            varNum: int
            additionalInfo: string
            exprTyp: string
            typ: string
            env: string
            children: ResizeArray<Node> 
        }
    
    let expr varNum exprTyp name env additionalInfo (children: Node list) =
        { 
            key = varNum
            name = name
            varNum = varNum
            additionalInfo = additionalInfo
            exprTyp = exprTyp
            typ = NodeTypes.expr
            env = env
            children = ResizeArray(children) 
        }

    let write (nodes: Node list) =
        let jsNodes =
            nodes
            |> List.map (fun n ->
                { 
                    key = n.key
                    name = n.name
                    varNum = $"{n.key}"
                    additionalInfo = n.additionalInfo
                    exprTyp = n.exprTyp
                    env = n.env
                    layout = n.typ 
                })
        let jsLinks =
            [
                for n in nodes do
                    for c in n.children do
                        { fromNode = n.key; toNode = c.key }
            ]

        writeData (serialize jsNodes) (serialize jsLinks) Layouts.tree
