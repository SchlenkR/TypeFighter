
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let id",
    "desc": "var = 1\ntype = ERROR (inherited)",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x)",
    "desc": "var = 3\ntype = (String -> String)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (x)",
    "desc": "var = 5\ntype = String",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Tuple",
    "desc": "var = 6\ntype = ERROR (inherited)",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "App",
    "desc": "var = 7\ntype = 'b",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var (id)",
    "desc": "var = 8\ntype = (String -> 'b)",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Lit (Hello World)",
    "desc": "var = 9\ntype = String",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "App",
    "desc": "var = 10\ntype = ERROR (inherited)",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Var (id)",
    "desc": "var = 11\ntype = ERROR: Cannot unify types \"(String -> String)\" and \"(Number -> 'd)\": Type mismatch",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Lit (42)",
    "desc": "var = 12\ntype = Number",
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
    "to": 3
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
    "from": 3,
    "to": 7
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 4,
    "to": 6
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
    