#r "nuget: Newtonsoft.Json"

open System
open System.IO
open Newtonsoft.Json

type NodeType = Constr | Var

type Node =
    { mutable key: int
      name: string
      desc: string
      typ: NodeType
      children: ResizeArray<Node> }

module Node =
    let constr name desc =
        { name = name
          desc = desc
          typ = Constr
          key = -1
          children = ResizeArray() }
    let var desc =
        { name = ""
          desc = desc
          typ = Var
          key = -1
          children = ResizeArray() }

    let connect (p: Node) c = p.children.Add c

let openGraph (nodes: Node list) =
    for i,n in nodes |> List.indexed do
        n.key <- i

    let jsNodes =
        nodes
        |> List.map (fun n ->
            {| key = n.key
               name = n.name
               desc = n.desc
               fig = match n.typ with | Constr -> "Procedure" | Var -> "Rectangle" |})
    let jsLinks =
        [
            for n in nodes do
                for c in n.children do
                    {| from = n.key; ``to`` = c.key |}
        ]
    
    let nodesJson = JsonConvert.SerializeObject(jsNodes, Formatting.Indented)
    let linksJson = JsonConvert.SerializeObject(jsLinks, Formatting.Indented)

    let json = $"window.nodeDataArray = {nodesJson}; \r\n\r\nwindow.linkDataArray = {linksJson};\r\n"

    let path = Path.Combine(__SOURCE_DIRECTORY__, "web")
    let dataPath = Path.Combine(path, "data.js")
    File.WriteAllText(dataPath, json)
    
module Test =
    open Node

    let n1 = constr "n 1" "constr 1"
    let n2 = constr "n 2" "constr 2"
    let n3 = constr "n 3" "constr 3"
    let n4 = var "constr 4"

    connect n1 n2
    connect n2 n3
    connect n2 n4 

    openGraph [ n1; n2; n3; n4 ]
