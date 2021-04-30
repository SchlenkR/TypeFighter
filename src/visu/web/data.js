
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Fun (x)",
    "desc": "type = ('a -> String)",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "App",
    "desc": "type = String",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (tostring)",
    "desc": "type = ('a -> String)",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Var (x)",
    "desc": "type = 'a",
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
    