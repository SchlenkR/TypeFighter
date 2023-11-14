#r "nuget: Newtonsoft.Json"

open System.IO
open Newtonsoft.Json

module private Layouts =
    let graph = "graph"
    let tree = "tree"

module NodeTypes =
    let var = "Rectangle"
    let op = "Ellipse"

type JsNode =
    { key: int
      name: string
      desc: string
      [<JsonProperty "fig">] layout: string }

type JsLink =
    { [<JsonProperty "from">] fromNode: int
      [<JsonProperty "to">] toNode: int }


let private writeData (nodesJson: string) (linksJson: string) (layout: string) =
    let json = $"
window.layout = \"{layout}\";
window.nodeDataArray = {nodesJson};
window.linkDataArray = {linksJson};
    "

    let path = Path.Combine(__SOURCE_DIRECTORY__, "src")
    let dataPath = Path.Combine(path, "data.js")
    File.WriteAllText(dataPath, json)

module Tree =

    type Node =
        { mutable key: int
          name: string
          desc: string
          typ: string
          children: ResizeArray<Node> }
    
    let var name desc (children: Node list) =
        { name = name
          desc = desc
          typ = NodeTypes.var
          key = -1
          children = ResizeArray(children) }

    let op desc (children: Node list) =
        { name = ""
          desc = desc
          typ = NodeTypes.op
          key = -1
          children = ResizeArray(children) }

    let connect (p: Node) c = p.children.Add c

    let write (nodes: Node list) =
        for i,n in nodes |> List.indexed do
            n.key <- i

        let jsNodes =
            nodes
            |> List.map (fun n ->
                { key = n.key
                  name = n.name
                  desc = n.desc
                  layout = n.typ })
        let jsLinks =
            [
                for n in nodes do
                    for c in n.children do
                        { fromNode = n.key; toNode = c.key }
            ]
    
        let nodesJson = JsonConvert.SerializeObject(jsNodes, Formatting.Indented)
        let linksJson = JsonConvert.SerializeObject(jsLinks, Formatting.Indented)

        writeData nodesJson linksJson Layouts.tree

module Graph =

    let write (jsNodes: JsNode list) (jsLinks: JsLink list) =
        let nodesJson = JsonConvert.SerializeObject(jsNodes, Formatting.Indented)
        let linksJson = JsonConvert.SerializeObject(jsLinks, Formatting.Indented)
    
        writeData nodesJson linksJson Layouts.graph


//module Test =
//    open TreeNode
//    let n1 = var "n 1" "constr 1" []
//    let n2 = var "n 2" "constr 2" []
//    let n3 = var "n 3" "constr 3" []
//    let n4 = constr "constr 4" []

//    connect n1 n2
//    connect n2 n3
//    connect n2 n4 

//    createTree [ n1; n2; n3; n4 ]
