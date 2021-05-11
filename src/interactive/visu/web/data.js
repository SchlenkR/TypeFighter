
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Fun (f)",
    "desc": "var = 1\ntype = ERROR (inherited)",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "App",
    "desc": "var = 3\ntype = ERROR (inherited)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (f)",
    "desc": "var = 4\ntype = 'a",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (42)",
    "desc": "var = 5\ntype = ERROR: Function type expected, but was: 'a",
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
    "from": 1,
    "to": 3
  }
];
    