
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "2 (Env)",
    "desc": "'a",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "4 (Var)",
    "desc": "'a",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Arg In",
    "desc": "'b",
    "fig": "Ellipse"
  },
  {
    "key": 3,
    "name": "SOURCE",
    "desc": "Number",
    "fig": "Ellipse"
  },
  {
    "key": 4,
    "name": "5 (Lit)",
    "desc": "Number",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Arg In",
    "desc": "'d",
    "fig": "Ellipse"
  },
  {
    "key": 6,
    "name": "Arg Out",
    "desc": "'g",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "UnifySubst",
    "desc": "'g",
    "fig": "Ellipse"
  },
  {
    "key": 8,
    "name": "3 (App)",
    "desc": "'g",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "MakeFun",
    "desc": "('a -> 'g)",
    "fig": "Ellipse"
  },
  {
    "key": 10,
    "name": "1 (Abs)",
    "desc": "('a -> 'g)",
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
    "from": 2,
    "to": 4
  },
  {
    "from": 1,
    "to": 5
  },
  {
    "from": 1,
    "to": 6
  },
  {
    "from": 4,
    "to": 7
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
    "from": 0,
    "to": 9
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 9,
    "to": 10
  }
];
    