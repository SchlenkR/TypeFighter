
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": -26,
    "name": "MakeTuple",
    "desc": "(String * Number)\ninsts = \n-  'a = 'd\n-  'd = 'e\n-  'd = 'f\nsubsts = \n-  'b = String\n-  'c = Number\n-  'e = Number\n-  'f = String",
    "fig": "Ellipse"
  },
  {
    "key": -24,
    "name": "ArgOut",
    "desc": "'c\ninsts = \n-  'a = 'd\n-  'd = 'e\nsubsts = \n-  'e = 'c\n-  'e = Number",
    "fig": "Ellipse"
  },
  {
    "key": -23,
    "name": "Unify",
    "desc": "(Number -> 'c)\ninsts = \n-  'a = 'd\n-  'd = 'e\nsubsts = \n-  'e = 'c\n-  'e = Number",
    "fig": "Ellipse"
  },
  {
    "key": -21,
    "name": "Inst (Var)",
    "desc": "('e -> 'e)\ninsts = \n-  'a = 'd\n-  'd = 'e\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -20,
    "name": "MakeFun",
    "desc": "(Number -> 'c)\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -19,
    "name": "SOURCE",
    "desc": "'c\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -17,
    "name": "SOURCE",
    "desc": "Number\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -15,
    "name": "ArgOut",
    "desc": "'b\ninsts = \n-  'a = 'd\n-  'd = 'f\nsubsts = \n-  'f = 'b\n-  'f = String",
    "fig": "Ellipse"
  },
  {
    "key": -14,
    "name": "Unify",
    "desc": "(String -> 'b)\ninsts = \n-  'a = 'd\n-  'd = 'f\nsubsts = \n-  'f = 'b\n-  'f = String",
    "fig": "Ellipse"
  },
  {
    "key": -12,
    "name": "Inst (Var)",
    "desc": "('f -> 'f)\ninsts = \n-  'a = 'd\n-  'd = 'f\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -11,
    "name": "MakeFun",
    "desc": "(String -> 'b)\ninsts = [ ]\nsubsts = [ ]",
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
    "desc": "String\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -5,
    "name": "MakeFun",
    "desc": "('a -> 'd)\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": -3,
    "name": "Inst (Var)",
    "desc": "'d\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
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
    "desc": "(String * Number)\ninsts = \n-  'a = 'd\n-  'd = 'e\n-  'd = 'f\nsubsts = \n-  'b = String\n-  'c = Number\n-  'e = Number\n-  'f = String",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "2 (Env (id))",
    "desc": "('d -> 'd)\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "3 (Abs)",
    "desc": "('d -> 'd)\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
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
    "desc": "'d\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "6 (Tuple)",
    "desc": "(String * Number)\ninsts = \n-  'a = 'd\n-  'd = 'e\n-  'd = 'f\nsubsts = \n-  'b = String\n-  'c = Number\n-  'e = Number\n-  'f = String",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "7 (App)",
    "desc": "String\ninsts = \n-  'a = 'd\n-  'd = 'f\nsubsts = \n-  'b = String\n-  'f = String",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "8 (Var)",
    "desc": "('f -> 'f)\ninsts = \n-  'a = 'd\n-  'd = 'f\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "9 (Lit)",
    "desc": "String\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "10 (App)",
    "desc": "Number\ninsts = \n-  'a = 'd\n-  'd = 'e\nsubsts = \n-  'c = Number\n-  'e = Number",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "11 (Var)",
    "desc": "('e -> 'e)\ninsts = \n-  'a = 'd\n-  'd = 'e\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 12,
    "name": "12 (Lit)",
    "desc": "Number\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  }
];
window.linkDataArray = [
  {
    "from": 7,
    "to": -26
  },
  {
    "from": 10,
    "to": -26
  },
  {
    "from": -23,
    "to": -24
  },
  {
    "from": 11,
    "to": -23
  },
  {
    "from": -20,
    "to": -23
  },
  {
    "from": 2,
    "to": -21
  },
  {
    "from": 12,
    "to": -20
  },
  {
    "from": -19,
    "to": -20
  },
  {
    "from": -14,
    "to": -15
  },
  {
    "from": 8,
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
    "from": 9,
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
    "from": -26,
    "to": 6
  },
  {
    "from": -15,
    "to": 7
  },
  {
    "from": -12,
    "to": 8
  },
  {
    "from": -8,
    "to": 9
  },
  {
    "from": -24,
    "to": 10
  },
  {
    "from": -21,
    "to": 11
  },
  {
    "from": -17,
    "to": 12
  }
];
    