function treeLayout() {

    var inst = {}

    inst.getLayout = function ($) {
        return $(go.TreeLayout,
            {
                angle: 90
            });
    }

    var hideWhenEmpty = function (prop) {
        return new go.Binding("height",
            prop,
            function (x) {
                if (x === "") return 0;
                else return undefined;
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
                        strokeWidth: 2
                    },
                    new go.Binding("figure", "fig")),
                $(go.Panel,
                    "Table",
                    {
                        defaultAlignment: go.Spot.Left,
                        margin: 10
                    },
                    $(go.RowColumnDefinition,
                        {
                            column: 1,
                            width: 1
                        }),
                    $(go.TextBlock,
                        {
                            row: 0,
                            alignment: go.Spot.Center,
                            font: "bold 12pt sans-serif"
                        },
                        hideWhenEmpty("name"),
                        new go.Binding("text", "name")),
                    $(go.TextBlock,
                        {
                            row: 1,
                            font: "10pt Consolas",
                            spacingAbove: 2,
                            spacingBelow: 2
                        },
                        hideWhenEmpty("desc"),
                        new go.Binding("text", "desc"))
                )
            );

        myDiagram.linkTemplate =
            $(go.Link, // the whole link panel
                $(go.Shape), // the link shape
                $(go.Shape, // the arrowhead
                    {
                        toArrow: "OpenTriangle",
                        fill: null
                    })
            );
    };

    return inst;
}
