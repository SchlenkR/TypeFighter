namespace rec TestData

open System

type BlockEdge(id: int) =
    member this.Id = id
    member this.Shipments = ResizeArray<Shipment>()

type Shipment(id: int) =
    member this.Id = id
    member this.BlockEdges = ResizeArray<BlockEdge>()

type Context() =
    let blockEdges = [ for x in 0..3 do BlockEdge x ]
    let shipments = [ for x in 0..3 do Shipment x ]

    do
        for be in blockEdges do
        for s in shipments do
            be.Shipments.Add s
            s.BlockEdges.Add be

    member this.BlockEdges = blockEdges |> List.toSeq
    member this.Shipments = shipments |> List.toSeq
    member this.RandomNumbers =
        let now = int DateTime.Now.Ticks
        seq { for x in 0..10 do now + x }

