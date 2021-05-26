
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": -15,
    "name": "ArgOut",
    "desc": "'b\ninsts = \n-  'a = 'c\n-  'c = 'd\nsubsts = \n-  'd = 'b\n-  'd = Number",
    "fig": "Ellipse"
  },
  {
    "key": -14,
    "name": "Unify",
    "desc": "(Number -> 'b)\ninsts = \n-  'a = 'c\n-  'c = 'd\nsubsts = \n-  'd = 'b\n-  'd = Number",
    "fig": "Ellipse"
  },
  {
    "key": -12,
    "name": "Inst (Var)",
    "desc": "('c -> 'c)\ninsts = \n-  'a = 'c\n-  'c = 'd\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -11,
    "name": "MakeFun",
    "desc": "(Number -> 'b)\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -10,
    "name": "SOURCE",
    "desc": "'b\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -8,
    "name": "SOURCE",
    "desc": "Number\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -5,
    "name": "MakeFun",
    "desc": "('a -> 'c)\ninsts = [ 'a = 'c ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -3,
    "name": "Inst (Var)",
    "desc": "'a\ninsts = [ 'a = 'c ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -1,
    "name": "SOURCE",
    "desc": "'a\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 1,
    "name": "1 (Let)",
    "desc": "Number\ninsts = \n-  'a = 'c\n-  'c = 'd\nsubsts = \n-  'b = Number\n-  'd = Number",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "2 (Env (id))",
    "desc": "('c -> 'c)\ninsts = [ 'a = 'c ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "3 (Abs)",
    "desc": "('c -> 'c)\ninsts = [ 'a = 'c ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "4 (Env (x))",
    "desc": "'a\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "5 (Var)",
    "desc": "'c\ninsts = [ 'a = 'c ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "6 (App)",
    "desc": "Number\ninsts = \n-  'a = 'c\n-  'c = 'd\nsubsts = \n-  'b = Number\n-  'd = Number",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "7 (Var)",
    "desc": "('d -> 'd)\ninsts = \n-  'a = 'c\n-  'c = 'd\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "8 (Lit)",
    "desc": "Number\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  }
];
window.linkDataArray = [
  {
    "from": -14,
    "to": -15
  },
  {
    "from": 7,
    "to": -14
  },
  {
    "from": -11,
    "to": -14
  },
  {
    "from": 2,
    "to": -12
  },
  {
    "from": 8,
    "to": -11
  },
  {
    "from": -10,
    "to": -11
  },
  {
    "from": 4,
    "to": -5
  },
  {
    "from": 5,
    "to": -5
  },
  {
    "from": 4,
    "to": -3
  },
  {
    "from": 6,
    "to": 1
  },
  {
    "from": 3,
    "to": 2
  },
  {
    "from": -5,
    "to": 3
  },
  {
    "from": -1,
    "to": 4
  },
  {
    "from": -3,
    "to": 5
  },
  {
    "from": -15,
    "to": 6
  },
  {
    "from": -12,
    "to": 7
  },
  {
    "from": -8,
    "to": 8
  }
];
    