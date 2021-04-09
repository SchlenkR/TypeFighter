#r "nuget: Newtonsoft.Json"

open System
open System.IO
open Newtonsoft.Json

module NodeTypes =
    let var = "Rectangle"
    let op = "Ellipse"

module Layouts =
    let graph = "graph"
    let tree = "tree"

type TreeNode =
    { mutable key: int
      name: string
      desc: string
      typ: string
      children: ResizeArray<TreeNode> }

module TreeNode =
    let var name desc (children: TreeNode list) =
        { name = name
          desc = desc
          typ = NodeTypes.var
          key = -1
          children = ResizeArray(children) }
    let op desc (children: TreeNode list) =
        { name = ""
          desc = desc
          typ = NodeTypes.op
          key = -1
          children = ResizeArray(children) }
    let connect (p: TreeNode) c = p.children.Add c

type JsNode =
    { key: int
      name: string
      desc: string
      [<JsonProperty "fig">] layout: string }

type JsLink =
    { [<JsonProperty "from">] fromNode: int
      [<JsonProperty "to">] toNode: int }

let writeGraph (jsNodes: JsNode list) (jsLinks: JsLink list) (layout: string) =
    let nodesJson = JsonConvert.SerializeObject(jsNodes, Formatting.Indented)
    let linksJson = JsonConvert.SerializeObject(jsLinks, Formatting.Indented)

    let json = $"
window.layout = \"{layout}\";
window.nodeDataArray = {nodesJson};
window.linkDataArray = {linksJson};
"

    let path = Path.Combine(__SOURCE_DIRECTORY__, "web")
    let dataPath = Path.Combine(path, "data.js")
    File.WriteAllText(dataPath, json)

let writeTree (nodes: TreeNode list) =
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
    
    writeGraph jsNodes jsLinks Layouts.tree



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
