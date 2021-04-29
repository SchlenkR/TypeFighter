
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "2 (Env)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "1 (Let)",
    "desc": "Seq<'b>",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "6 (Var)",
    "desc": "(Seq<'a> -> (('a -> 'b) -> Seq<'b>))",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "SOURCE",
    "desc": "(Seq<'a> -> (('a -> 'b) -> Seq<'b>))",
    "fig": "Ellipse"
  },
  {
    "key": 4,
    "name": "7 (Var)",
    "desc": "Seq<Number>",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "SOURCE",
    "desc": "Seq<Number>",
    "fig": "Ellipse"
  },
  {
    "key": 6,
    "name": "5 (App)",
    "desc": "(('a -> 'b) -> Seq<'b>)",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Arg Out",
    "desc": "(('a -> 'b) -> Seq<'b>)",
    "fig": "Ellipse"
  },
  {
    "key": 8,
    "name": "Arg In",
    "desc": "Seq<'a>",
    "fig": "Ellipse"
  },
  {
    "key": 9,
    "name": "9 (Env)",
    "desc": "'e",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "8 (Abs)",
    "desc": "ERROR: Cannot unity types \"('a -> 'b)\" and \"(Number -> 'e)\": TODO",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "12 (Var)",
    "desc": "(Number -> (Number -> Number))",
    "fig": "Rectangle"
  },
  {
    "key": 12,
    "name": "SOURCE",
    "desc": "(Number -> (Number -> Number))",
    "fig": "Ellipse"
  },
  {
    "key": 13,
    "name": "13 (Var)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "11 (App)",
    "desc": "(Number -> Number)",
    "fig": "Rectangle"
  },
  {
    "key": 15,
    "name": "Arg Out",
    "desc": "(Number -> Number)",
    "fig": "Ellipse"
  },
  {
    "key": 16,
    "name": "Arg In",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 17,
    "name": "14 (Var)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 18,
    "name": "10 (App)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 19,
    "name": "Arg Out",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 20,
    "name": "Arg In",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 21,
    "name": "MakeFun",
    "desc": "(Number -> 'e)",
    "fig": "Ellipse"
  },
  {
    "key": 22,
    "name": "4 (App)",
    "desc": "Seq<'b>",
    "fig": "Rectangle"
  },
  {
    "key": 23,
    "name": "Arg Out",
    "desc": "Seq<'b>",
    "fig": "Ellipse"
  },
  {
    "key": 24,
    "name": "Arg In",
    "desc": "('a -> 'b)",
    "fig": "Ellipse"
  },
  {
    "key": 25,
    "name": "3 (Lit)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 26,
    "name": "SOURCE",
    "desc": "Number",
    "fig": "Ellipse"
  }
];
window.linkDataArray = [
  {
    "from": 25,
    "to": 0
  },
  {
    "from": 22,
    "to": 1
  },
  {
    "from": 3,
    "to": 2
  },
  {
    "from": 8,
    "to": 4
  },
  {
    "from": 5,
    "to": 4
  },
  {
    "from": 7,
    "to": 6
  },
  {
    "from": 2,
    "to": 7
  },
  {
    "from": 2,
    "to": 8
  },
  {
    "from": 24,
    "to": 10
  },
  {
    "from": 21,
    "to": 10
  },
  {
    "from": 12,
    "to": 11
  },
  {
    "from": 16,
    "to": 13
  },
  {
    "from": 9,
    "to": 13
  },
  {
    "from": 15,
    "to": 14
  },
  {
    "from": 11,
    "to": 15
  },
  {
    "from": 11,
    "to": 16
  },
  {
    "from": 20,
    "to": 17
  },
  {
    "from": 0,
    "to": 17
  },
  {
    "from": 19,
    "to": 18
  },
  {
    "from": 14,
    "to": 19
  },
  {
    "from": 14,
    "to": 20
  },
  {
    "from": 18,
    "to": 21
  },
  {
    "from": 9,
    "to": 21
  },
  {
    "from": 23,
    "to": 22
  },
  {
    "from": 6,
    "to": 23
  },
  {
    "from": 6,
    "to": 24
  },
  {
    "from": 26,
    "to": 25
  }
];
    