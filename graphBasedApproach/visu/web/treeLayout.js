function treeLayout() {

    var inst = {}

    inst.getLayout = function ($) {
        return $(go.TreeLayout,
            {
                angle: 90
            });
    }

    inst.init = function ($, myDiagram) {
        myDiagram.nodeTemplate =
            $(go.Node,
                "Auto",
                $(go.Shape,
                    {
                        fill: "white",
                        stroke: "darkblue",
                        strokeWidth: 2,
                        figure: "Rectangle"
                    }),
                $(go.Panel,
                    "Table",
                    {
                        defaultAlignment: go.Spot.Left,
                        margin: 10
                    },
                    $(go.TextBlock,
                        {
                            row: 0,
                            alignment: go.Spot.Center,
                            font: "bold 12pt sans-serif",
                            spacingBelow: 10
                        },
                        new go.Binding("text", "name")),
                    $(go.TextBlock,
                        {
                            row: 1,
                            font: "10pt Consolas",
                            spacingAbove: 4
                        },
                        new go.Binding("text", "desc"))
                )
            );
    };

    return inst;
}
