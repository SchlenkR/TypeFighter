
// UseCase: parent-child: Shipment -> ReallocationBlockEdges
// requested parent IDs: [ 2; 3; 4 ]
query {
    let parentIds = [ 2; 3; 4 ]
    for shipment in context.Shipments do
    where (parentIds.Contains(shipment.Id))
    for blockEdge in shipment.ReallocationBlockEdges do
    select 
        {|
            Id = makeId(shipment.Id, blockEdge.Id)
            Prop1 = shipment.Prop1
            Prop2 = blockEdge.Prop2
        |}
}


// UseCase: BlockEdge im Szenariovergleich
// keine ID-Einschränkung
// Matching-Properties sind serverseitig konfiguriert:  
//            BlockEdge: [ be.FachId; be.Sonstwas ]

query {
    for blockEdge in context.BlockEdges do
    let compColumn1Scen1, compColumn1Scen2 = join(blockEdge.Length, "PsUrl1")
    let compColumn2Scen1, compColumn1Scen2 = join(blockEdge.Length, "PsUrl1")
    select
        {|
            Id = blockEdge.Id
            Prop2 = blockEdge.Prop2
        |}
    skip 300
    take 101
}
