
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "App",
    "desc": "var = 1\ntype = Number",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (__)",
    "desc": "var = 2\ntype = ('a -> 'a)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (__)",
    "desc": "var = 4\ntype = 'a",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (0)",
    "desc": "var = 5\ntype = Number",
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
  }
];
    