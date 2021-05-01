
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "App",
    "desc": "type = ERROR: Inherit",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "App",
    "desc": "type = (Seq<Number> -> Seq<Number>)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (cons)",
    "desc": "type = ('a -> (Seq<'a> -> Seq<'a>))",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (1)",
    "desc": "type = Number",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "App",
    "desc": "type = ERROR: Inherit",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "App",
    "desc": "type = (Seq<Number> -> Seq<Number>)",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Var (cons)",
    "desc": "type = ('a -> (Seq<'a> -> Seq<'a>))",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Lit (2)",
    "desc": "type = Number",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "App",
    "desc": "type = ERROR: Origin \"Cannot unify types \"Seq<String>\" and \"Seq<Number>\": Type mismatch\"",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "App",
    "desc": "type = (Seq<String> -> Seq<String>)",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Var (cons)",
    "desc": "type = ('a -> (Seq<'a> -> Seq<'a>))",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Lit (xxx)",
    "desc": "type = String",
    "fig": "Rectangle"
  },
  {
    "key": 12,
    "name": "Var (emptyList)",
    "desc": "type = Seq<String>",
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
    "to": 4
  },
  {
    "from": 1,
    "to": 2
  },
  {
    "from": 1,
    "to": 3
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 4,
    "to": 8
  },
  {
    "from": 5,
    "to": 6
  },
  {
    "from": 5,
    "to": 7
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 8,
    "to": 12
  },
  {
    "from": 9,
    "to": 10
  },
  {
    "from": 9,
    "to": 11
  }
];
    