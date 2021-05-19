
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "SOURCE",
    "desc": "'a\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 1,
    "name": "4 (Env (x))",
    "desc": "'a\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Inst (Var)",
    "desc": "'a\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 3,
    "name": "5 (Var)",
    "desc": "'a\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "MakeFun",
    "desc": "('a -> 'a)\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 5,
    "name": "3 (Abs)",
    "desc": "('a -> 'a)\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "2 (Env (id))",
    "desc": "('a -> 'a)\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "SOURCE",
    "desc": "String\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 8,
    "name": "9 (Lit)",
    "desc": "String\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "SOURCE",
    "desc": "'b\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 10,
    "name": "MakeFun",
    "desc": "(String -> 'b)\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 11,
    "name": "Inst (Var)",
    "desc": "('a -> 'a)\ninsts = \n-  'a = 'd\n-  'a = 'e\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 12,
    "name": "8 (Var)",
    "desc": "('a -> 'a)\ninsts = \n-  'a = 'd\n-  'a = 'e\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 13,
    "name": "Unify",
    "desc": "(String -> 'b)\ninsts = \n-  'a = 'd\n-  'a = 'e\nsubsts = \n-  'a = 'b\n-  'a = String",
    "fig": "Ellipse"
  },
  {
    "key": 14,
    "name": "ArgOut",
    "desc": "'b\ninsts = \n-  'a = 'd\n-  'a = 'e\nsubsts = \n-  'a = 'b\n-  'a = String",
    "fig": "Ellipse"
  },
  {
    "key": 15,
    "name": "7 (App)",
    "desc": "'b\ninsts = \n-  'a = 'd\n-  'a = 'e\nsubsts = \n-  'a = 'b\n-  'a = String",
    "fig": "Rectangle"
  },
  {
    "key": 16,
    "name": "SOURCE",
    "desc": "Number\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 17,
    "name": "12 (Lit)",
    "desc": "Number\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 18,
    "name": "SOURCE",
    "desc": "'c\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 19,
    "name": "MakeFun",
    "desc": "(Number -> 'c)\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 20,
    "name": "Inst (Var)",
    "desc": "('a -> 'a)\ninsts = \n-  'a = 'd\n-  'a = 'f\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 21,
    "name": "11 (Var)",
    "desc": "('a -> 'a)\ninsts = \n-  'a = 'd\n-  'a = 'f\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 22,
    "name": "Unify",
    "desc": "(Number -> 'c)\ninsts = \n-  'a = 'd\n-  'a = 'f\nsubsts = \n-  'a = 'c\n-  'a = Number",
    "fig": "Ellipse"
  },
  {
    "key": 23,
    "name": "ArgOut",
    "desc": "'c\ninsts = \n-  'a = 'd\n-  'a = 'f\nsubsts = \n-  'a = 'c\n-  'a = Number",
    "fig": "Ellipse"
  },
  {
    "key": 24,
    "name": "10 (App)",
    "desc": "'c\ninsts = \n-  'a = 'd\n-  'a = 'f\nsubsts = \n-  'a = 'c\n-  'a = Number",
    "fig": "Rectangle"
  },
  {
    "key": 25,
    "name": "MakeTuple",
    "desc": "('b * 'c)\ninsts = \n-  'a = 'd\n-  'a = 'e\n-  'a = 'f\nsubsts = \n-  'a = 'b\n-  'a = 'c\n-  'a = Number\n-  'a = String",
    "fig": "Ellipse"
  },
  {
    "key": 26,
    "name": "6 (Tuple)",
    "desc": "('b * 'c)\ninsts = \n-  'a = 'd\n-  'a = 'e\n-  'a = 'f\nsubsts = \n-  'a = 'b\n-  'a = 'c\n-  'a = Number\n-  'a = String",
    "fig": "Rectangle"
  },
  {
    "key": 27,
    "name": "1 (Let)",
    "desc": "('b * 'c)\ninsts = \n-  'a = 'd\n-  'a = 'e\n-  'a = 'f\nsubsts = \n-  'a = 'b\n-  'a = 'c\n-  'a = Number\n-  'a = String",
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
    "from": 2,
    "to": 3
  },
  {
    "from": 1,
    "to": 4
  },
  {
    "from": 3,
    "to": 4
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
    "from": 7,
    "to": 8
  },
  {
    "from": 8,
    "to": 10
  },
  {
    "from": 9,
    "to": 10
  },
  {
    "from": 6,
    "to": 11
  },
  {
    "from": 11,
    "to": 12
  },
  {
    "from": 12,
    "to": 13
  },
  {
    "from": 10,
    "to": 13
  },
  {
    "from": 13,
    "to": 14
  },
  {
    "from": 14,
    "to": 15
  },
  {
    "from": 16,
    "to": 17
  },
  {
    "from": 17,
    "to": 19
  },
  {
    "from": 18,
    "to": 19
  },
  {
    "from": 6,
    "to": 20
  },
  {
    "from": 20,
    "to": 21
  },
  {
    "from": 21,
    "to": 22
  },
  {
    "from": 19,
    "to": 22
  },
  {
    "from": 22,
    "to": 23
  },
  {
    "from": 23,
    "to": 24
  },
  {
    "from": 15,
    "to": 25
  },
  {
    "from": 24,
    "to": 25
  },
  {
    "from": 25,
    "to": 26
  },
  {
    "from": 26,
    "to": 27
  }
];
    