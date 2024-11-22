import * as go from 'gojs';

const nonBreakingSpace = '\u00A0';

function treeLayout() {
  var inst = {};

  inst.getLayout = function ($) {
    return $(go.TreeLayout, {
      angle: 90,
    });
  };

  inst.init = function ($, myDiagram) {
    myDiagram.nodeTemplate = $(
      go.Node,
      'Auto',
      $(go.Shape, {
        fill: 'white',
        stroke: 'darkblue',
        strokeWidth: 2,
        figure: 'Rectangle',
      }),
      $(
        go.Panel,
        'Table',
        {
          defaultAlignment: go.Spot.Left,
          margin: 10,
        },
        $(
          go.Panel,
          'Table',
          {
            row: 0,
            defaultAlignment: go.Spot.Left,
            margin: 10,
          },
          $(
            go.TextBlock,
            {
              column: 0,
              alignment: go.Spot.Center,
              font: 'bold 12pt sans-serif',
            },
            new go.Binding('text', 'name')
          ),
          $(
            go.TextBlock,
            {
              column: 1,
              alignment: go.Spot.Center,
              margin: new go.Margin(0, 0, 0, 10),
            },
            new go.Binding('text', 'varNum', v => "tv_" + v)
          )
        ),

        $(
          go.TextBlock,
          {
            row: 1,
            font: '10pt Consolas',
            spacingAbove: 4,
          },
          new go.Binding('text', 'additionalInfo', v => ".           " + v),
          {
            stroke: 'gray',
            font: 'italic 10pt Consolas',
          },
          new go.Binding('visible', 'additionalInfo', v => v !== '')
        ),

        $(
          go.TextBlock,
          {
            row: 2,
            font: '10pt Consolas',
            spacingAbove: 4,
          },
          new go.Binding('text', 'exprTyp'),
          new go.Binding('visible', 'exprTyp', v => v !== '')
        )

        // $('PanelExpanderButton', 'INFO', { row: 3 }),
        // $(
        //   go.Panel,
        //   'Vertical',
        //   {
        //     name: 'INFO', // identify to the PanelExpanderButton
        //     margin: 8,
        //     row: 4,
        //     defaultAlignment: go.Spot.Left,
        //   },
        //   $(go.TextBlock, new go.Binding('text', 'env'))
        // )
      )
    );
  };

  return inst;
}

function init() {
  var $ = go.GraphObject.make; // for conciseness in defining templates

  var inst;
  if (window.layout === 'graph') {
    inst = graphLayout($);
  } else {
    inst = treeLayout($);
  }

  var myDiagram = $(
    go.Diagram,
    'diagramDiv', // must be the ID or reference to div
    {
      layout: inst.getLayout($),
      initialAutoScale: go.Diagram.Uniform,
      initialContentAlignment: go.Spot.Center,
      'toolManager.mouseWheelBehavior': go.ToolManager.WheelZoom,
    }
  );

  // default
  myDiagram.linkTemplate = $(
    go.Link, // the whole link panel
    $(go.Shape), // the link shape
    $(
      go.Shape, // the arrowhead
      {
        toArrow: 'OpenTriangle',
        fill: null,
      }
    )
  );

  inst.init($, myDiagram);

  myDiagram.animationManager.initialAnimationStyle = go.AnimationManager.None;
  myDiagram.model = new go.GraphLinksModel(
    window.nodeDataArray,
    window.linkDataArray
  );
  window.myDiagram = myDiagram;
}

init();
