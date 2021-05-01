
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "App",
    "desc": "var = 1\ntype = String",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Var (tostring)",
    "desc": "var = 2\ntype = ('a -> String)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "App",
    "desc": "var = 3\ntype = Number",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "App",
    "desc": "var = 4\ntype = (Number -> Number)",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var (mul)",
    "desc": "var = 5\ntype = (Number -> (Number -> Number))",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Lit (2)",
    "desc": "var = 6\ntype = Number",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "App",
    "desc": "var = 7\ntype = Number",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "App",
    "desc": "var = 8\ntype = (Number -> Number)",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Var (add)",
    "desc": "var = 9\ntype = (Number -> (Number -> Number))",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Lit (3)",
    "desc": "var = 10\ntype = Number",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Lit (10)",
    "desc": "var = 11\ntype = Number",
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
    "from": 6,
    "to": 10
  },
  {
    "from": 7,
    "to": 8
  },
  {
    "from": 7,
    "to": 9
  }
];
    