
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let id = ...",
    "desc": "",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x) ->",
    "desc": "",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (x)",
    "desc": "",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "App",
    "desc": "",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var (id)",
    "desc": "",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Lit (1)",
    "desc": "",
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
    "to": 5
  }
];
    