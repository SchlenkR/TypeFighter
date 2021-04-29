
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "SOURCE",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 1,
    "name": "3 (Lit)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "2 (Env)",
    "desc": "Number",
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
    "name": "6 (Var)",
    "desc": "(Seq<'a> -> (('a -> 'b) -> Seq<'b>))",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Arg In",
    "desc": "Seq<'a>",
    "fig": "Ellipse"
  },
  {
    "key": 6,
    "name": "SOURCE",
    "desc": "Seq<Number>",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "7 (Var)",
    "desc": "Seq<Number>",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Arg Out",
    "desc": "(('a -> 'b) -> Seq<'b>)",
    "fig": "Ellipse"
  },
  {
    "key": 9,
    "name": "5 (App)",
    "desc": "(('a -> 'b) -> Seq<'b>)",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Arg In",
    "desc": "('a -> 'b)",
    "fig": "Ellipse"
  },
  {
    "key": 11,
    "name": "9 (Env)",
    "desc": "('a -> 'b)",
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
    "name": "12 (Var)",
    "desc": "(Number -> (Number -> Number))",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "Arg In",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 15,
    "name": "13 (Var)",
    "desc": "ERROR: Cannot unity types \"('a -> 'b)\" and \"Number\": TODO",
    "fig": "Rectangle"
  },
  {
    "key": 16,
    "name": "Arg Out",
    "desc": "(Number -> Number)",
    "fig": "Ellipse"
  },
  {
    "key": 17,
    "name": "11 (App)",
    "desc": "(Number -> Number)",
    "fig": "Rectangle"
  },
  {
    "key": 18,
    "name": "Arg In",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 19,
    "name": "14 (Var)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 20,
    "name": "Arg Out",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 21,
    "name": "10 (App)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 22,
    "name": "MakeFun",
    "desc": "(('a -> 'b) -> Number)",
    "fig": "Ellipse"
  },
  {
    "key": 23,
    "name": "8 (Abs)",
    "desc": "ERROR: Cannot unity types \"(('a -> 'b) -> Number)\" and \"('a -> 'b)\": TODO",
    "fig": "Rectangle"
  },
  {
    "key": 24,
    "name": "Arg Out",
    "desc": "Seq<'b>",
    "fig": "Ellipse"
  },
  {
    "key": 25,
    "name": "4 (App)",
    "desc": "Seq<'b>",
    "fig": "Rectangle"
  },
  {
    "key": 26,
    "name": "1 (Let)",
    "desc": "Seq<'b>",
    "fig": "Rectangle"
  }
];
window.linkDataArray = [
  {
    "from": 0,
    "to": 1
  },
  {
    "from": 1,
    "to": 2
  },
  {
    "from": 3,
    "to": 4
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 6,
    "to": 7
  },
  {
    "from": 5,
    "to": 7
  },
  {
    "from": 4,
    "to": 8
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 9,
    "to": 10
  },
  {
    "from": 10,
    "to": 11
  },
  {
    "from": 12,
    "to": 13
  },
  {
    "from": 13,
    "to": 14
  },
  {
    "from": 11,
    "to": 15
  },
  {
    "from": 14,
    "to": 15
  },
  {
    "from": 13,
    "to": 16
  },
  {
    "from": 16,
    "to": 17
  },
  {
    "from": 17,
    "to": 18
  },
  {
    "from": 2,
    "to": 19
  },
  {
    "from": 18,
    "to": 19
  },
  {
    "from": 17,
    "to": 20
  },
  {
    "from": 20,
    "to": 21
  },
  {
    "from": 11,
    "to": 22
  },
  {
    "from": 21,
    "to": 22
  },
  {
    "from": 22,
    "to": 23
  },
  {
    "from": 10,
    "to": 23
  },
  {
    "from": 9,
    "to": 24
  },
  {
    "from": 24,
    "to": 25
  },
  {
    "from": 25,
    "to": 26
  }
];
    