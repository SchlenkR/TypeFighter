
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "SOURCE",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 1,
    "name": "5 (Lit)",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "SOURCE",
    "desc": "'a\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 3,
    "name": "MakeFun",
    "desc": "(Number -> 'a)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 4,
    "name": "SOURCE",
    "desc": "'b\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 5,
    "name": "3 (Env (x))",
    "desc": "'b\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Inst (Var)",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "4 (Var)",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "MakeFun",
    "desc": "('b -> 'c)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 9,
    "name": "2 (Abs)",
    "desc": "('b -> 'c)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Unify",
    "desc": "(Number -> 'a)\nsubsts = \n-  'b = Number\n-  'c = 'a",
    "fig": "Ellipse"
  },
  {
    "key": 11,
    "name": "ArgOut",
    "desc": "'a\nsubsts = \n-  'b = Number\n-  'c = 'a",
    "fig": "Ellipse"
  },
  {
    "key": 12,
    "name": "1 (App)",
    "desc": "'a\nsubsts = \n-  'b = Number\n-  'c = 'a",
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
    "to": 3
  },
  {
    "from": 2,
    "to": 3
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 5,
    "to": 6
  },
  {
    "from": 6,
    "to": 7
  },
  {
    "from": 5,
    "to": 8
  },
  {
    "from": 7,
    "to": 8
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 9,
    "to": 10
  },
  {
    "from": 3,
    "to": 10
  },
  {
    "from": 10,
    "to": 11
  },
  {
    "from": 11,
    "to": 12
  }
];
    