
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
        code: string
        varNum: string
        additionalInfo: string
        exprTyp: string
        env: string
    }

type JsLink =
    { 
        [<JsonPropertyName("from")>] fromNode: int
        [<JsonPropertyName("to")>] toNode: int 
    }

[<AutoOpen>]
module internal Internal =
    let writeData (nodesJson: string) (linksJson: string) =
        let json = $"
window.nodeDataArray = {nodesJson};
window.linkDataArray = {linksJson};
        "

        let path = Path.Combine(__SOURCE_DIRECTORY__, "data")
        let dataPath = Path.Combine(path, "data.js")
        File.WriteAllText(dataPath, json)

    let serialize (v: obj) =
        JsonSerializer.Serialize(v, JsonSerializerOptions(WriteIndented = true))

[<RequireQualifiedAccess>]
module Tree =
    type Node =
        { 
            mutable key: int
            name: string
            code: string
            varNum: int
            additionalInfo: string
            exprTyp: string
            env: string
            children: ResizeArray<Node> 
        }
    
    let expr varNum code exprTyp name env additionalInfo (children: Node list) =
        { 
            key = varNum
            name = name
            code = code
            varNum = varNum
            additionalInfo = additionalInfo
            exprTyp = exprTyp
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
                    code = n.code
                    varNum = $"{n.key}"
                    additionalInfo = n.additionalInfo
                    exprTyp = n.exprTyp
                    env = n.env
                })
        let jsLinks =
            [
                for n in nodes do
                    for c in n.children do
                        { fromNode = n.key; toNode = c.key }
            ]

        writeData (serialize jsNodes) (serialize jsLinks)
