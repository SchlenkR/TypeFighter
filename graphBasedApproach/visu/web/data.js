
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "let x = 'e1' : 3 in 'e2' : 4",
    "desc": "Seq<Number>",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Lit (10: Number)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "App",
    "desc": "Seq<Number>",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "App",
    "desc": "((Number -> 'b) -> Seq<'b>)",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var 'map' : (Seq<'a> -> (('a -> 'b) -> Seq<'b>))",
    "desc": "(Seq<'a> -> (('a -> 'b) -> Seq<'b>))",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var 'Numbers' : Seq<Number>",
    "desc": "Seq<Number>",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "fun 'number' : 9 -> 'e' : 10",
    "desc": "(Number -> Number)",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "App",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "App",
    "desc": "(Number -> Number)",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Var 'add' : (Number -> (Number -> Number))",
    "desc": "(Number -> (Number -> Number))",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Var 'number' : 9",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Var 'x' : 2",
    "desc": "Number",
    "fig": "Rectangle"
  }
];
window.linkDataArray = [
  {
    "from": 0,
    "to": 1
  },
  {
    "from": 0,
    "to": 2
  },
  {
    "from": 2,
    "to": 3
  },
  {
    "from": 2,
    "to": 6
  },
  {
    "from": 3,
    "to": 4
  },
  {
    "from": 3,
    "to": 5
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
    "from": 7,
    "to": 11
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 8,
    "to": 10
  }
];
    