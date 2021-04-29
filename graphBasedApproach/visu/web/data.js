
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
    "name": "SOURCE",
    "desc": "'p",
    "fig": "Ellipse"
  },
  {
    "key": 2,
    "name": "3 (Lit)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "SOURCE",
    "desc": "'q",
    "fig": "Ellipse"
  },
  {
    "key": 4,
    "name": "2 (Env)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "SOURCE",
    "desc": "(Seq<'a> -> (('a -> 'b) -> Seq<'b>))",
    "fig": "Ellipse"
  },
  {
    "key": 6,
    "name": "SOURCE",
    "desc": "'r",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "6 (Var)",
    "desc": "(Seq<'a> -> (('a -> 'b) -> Seq<'b>))",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Arg In",
    "desc": "Seq<'a>",
    "fig": "Ellipse"
  },
  {
    "key": 9,
    "name": "SOURCE",
    "desc": "Seq<Number>",
    "fig": "Ellipse"
  },
  {
    "key": 10,
    "name": "7 (Var)",
    "desc": "Seq<Number>",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Arg Out",
    "desc": "(('a -> 'b) -> Seq<'b>)",
    "fig": "Ellipse"
  },
  {
    "key": 12,
    "name": "SOURCE",
    "desc": "'s",
    "fig": "Ellipse"
  },
  {
    "key": 13,
    "name": "5 (App)",
    "desc": "(('a -> 'b) -> Seq<'b>)",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "Arg In",
    "desc": "('a -> 'b)",
    "fig": "Ellipse"
  },
  {
    "key": 15,
    "name": "SOURCE",
    "desc": "'t",
    "fig": "Ellipse"
  },
  {
    "key": 16,
    "name": "SOURCE",
    "desc": "'u",
    "fig": "Ellipse"
  },
  {
    "key": 17,
    "name": "9 (Env)",
    "desc": "'u",
    "fig": "Rectangle"
  },
  {
    "key": 18,
    "name": "SOURCE",
    "desc": "(Number -> (Number -> Number))",
    "fig": "Ellipse"
  },
  {
    "key": 19,
    "name": "SOURCE",
    "desc": "'v",
    "fig": "Ellipse"
  },
  {
    "key": 20,
    "name": "12 (Var)",
    "desc": "(Number -> (Number -> Number))",
    "fig": "Rectangle"
  },
  {
    "key": 21,
    "name": "Arg In",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 22,
    "name": "13 (Var)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 23,
    "name": "Arg Out",
    "desc": "(Number -> Number)",
    "fig": "Ellipse"
  },
  {
    "key": 24,
    "name": "SOURCE",
    "desc": "'w",
    "fig": "Ellipse"
  },
  {
    "key": 25,
    "name": "11 (App)",
    "desc": "(Number -> Number)",
    "fig": "Rectangle"
  },
  {
    "key": 26,
    "name": "Arg In",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 27,
    "name": "14 (Var)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 28,
    "name": "Arg Out",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 29,
    "name": "SOURCE",
    "desc": "'x",
    "fig": "Ellipse"
  },
  {
    "key": 30,
    "name": "10 (App)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 31,
    "name": "MakeFun",
    "desc": "('u -> Number)",
    "fig": "Ellipse"
  },
  {
    "key": 32,
    "name": "8 (Abs)",
    "desc": "ERROR: Cannot unity types \"('u -> Number)\" and \"('a -> 'b)\": TODO",
    "fig": "Rectangle"
  },
  {
    "key": 33,
    "name": "Arg Out",
    "desc": "Seq<'b>",
    "fig": "Ellipse"
  },
  {
    "key": 34,
    "name": "SOURCE",
    "desc": "'y",
    "fig": "Ellipse"
  },
  {
    "key": 35,
    "name": "4 (App)",
    "desc": "Seq<'b>",
    "fig": "Rectangle"
  },
  {
    "key": 36,
    "name": "SOURCE",
    "desc": "'z",
    "fig": "Ellipse"
  },
  {
    "key": 37,
    "name": "1 (Let)",
    "desc": "Seq<'b>",
    "fig": "Rectangle"
  }
];
window.linkDataArray = [
  {
    "from": 0,
    "to": 2
  },
  {
    "from": 1,
    "to": 2
  },
  {
    "from": 2,
    "to": 4
  },
  {
    "from": 3,
    "to": 4
  },
  {
    "from": 5,
    "to": 7
  },
  {
    "from": 6,
    "to": 7
  },
  {
    "from": 7,
    "to": 8
  },
  {
    "from": 9,
    "to": 10
  },
  {
    "from": 8,
    "to": 10
  },
  {
    "from": 7,
    "to": 11
  },
  {
    "from": 11,
    "to": 13
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
    "from": 15,
    "to": 17
  },
  {
    "from": 16,
    "to": 17
  },
  {
    "from": 18,
    "to": 20
  },
  {
    "from": 19,
    "to": 20
  },
  {
    "from": 20,
    "to": 21
  },
  {
    "from": 17,
    "to": 22
  },
  {
    "from": 21,
    "to": 22
  },
  {
    "from": 20,
    "to": 23
  },
  {
    "from": 23,
    "to": 25
  },
  {
    "from": 24,
    "to": 25
  },
  {
    "from": 25,
    "to": 26
  },
  {
    "from": 4,
    "to": 27
  },
  {
    "from": 26,
    "to": 27
  },
  {
    "from": 25,
    "to": 28
  },
  {
    "from": 28,
    "to": 30
  },
  {
    "from": 29,
    "to": 30
  },
  {
    "from": 17,
    "to": 31
  },
  {
    "from": 30,
    "to": 31
  },
  {
    "from": 31,
    "to": 32
  },
  {
    "from": 14,
    "to": 32
  },
  {
    "from": 13,
    "to": 33
  },
  {
    "from": 33,
    "to": 35
  },
  {
    "from": 34,
    "to": 35
  },
  {
    "from": 35,
    "to": 37
  },
  {
    "from": 36,
    "to": 37
  }
];
    