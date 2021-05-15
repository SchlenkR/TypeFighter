
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "SOURCE",
    "desc": "'a\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 1,
    "name": "4 (Env (x))",
    "desc": "'a\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "5 (Var)",
    "desc": "'a\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Inst (Var)",
    "desc": "'a\nsubsts = [ 'a = 'd ]",
    "fig": "Ellipse"
  },
  {
    "key": 4,
    "name": "MakeFun",
    "desc": "('a -> 'a)\nsubsts = [ 'a = 'd ]",
    "fig": "Ellipse"
  },
  {
    "key": 5,
    "name": "3 (Abs)",
    "desc": "('d -> 'd)\nsubsts = [ 'a = 'd ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Inst (Abs)",
    "desc": "('d -> 'd)\nsubsts = \n-  'a = 'd\n-  'd = 'e",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "2 (Env (id))",
    "desc": "('e -> 'e)\nsubsts = \n-  'a = 'd\n-  'd = 'e",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "SOURCE",
    "desc": "String\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 9,
    "name": "9 (Lit)",
    "desc": "String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Inst (Lit)",
    "desc": "String\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 11,
    "name": "SOURCE",
    "desc": "'b\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 12,
    "name": "MakeFun",
    "desc": "(String -> 'b)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 13,
    "name": "8 (Var)",
    "desc": "('e -> 'e)\nsubsts = \n-  'a = 'd\n-  'd = 'e",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "Inst (Var)",
    "desc": "('e -> 'e)\nsubsts = \n-  'a = 'd\n-  'd = 'e\n-  'e = 'f",
    "fig": "Ellipse"
  },
  {
    "key": 15,
    "name": "Unify",
    "desc": "(String -> 'b)\nsubsts = \n-  'a = 'd\n-  'd = 'e\n-  'e = 'b\n-  'e = 'f\n-  'e = String",
    "fig": "Ellipse"
  },
  {
    "key": 16,
    "name": "Arg Out",
    "desc": "'b\nsubsts = \n-  'a = 'd\n-  'd = 'e\n-  'e = 'b\n-  'e = 'f\n-  'e = String",
    "fig": "Ellipse"
  },
  {
    "key": 17,
    "name": "7 (App)",
    "desc": "String\nsubsts = \n-  'a = 'd\n-  'b = 'f\n-  'd = 'e\n-  'e = String\n-  'f = String",
    "fig": "Rectangle"
  },
  {
    "key": 18,
    "name": "Inst (App)",
    "desc": "String\nsubsts = \n-  'a = 'd\n-  'b = 'f\n-  'd = 'e\n-  'e = String\n-  'f = String",
    "fig": "Ellipse"
  },
  {
    "key": 19,
    "name": "SOURCE",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 20,
    "name": "12 (Lit)",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 21,
    "name": "Inst (Lit)",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 22,
    "name": "SOURCE",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 23,
    "name": "MakeFun",
    "desc": "(Number -> 'c)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 24,
    "name": "11 (Var)",
    "desc": "('e -> 'e)\nsubsts = \n-  'a = 'd\n-  'd = 'e",
    "fig": "Rectangle"
  },
  {
    "key": 25,
    "name": "Inst (Var)",
    "desc": "('e -> 'e)\nsubsts = \n-  'a = 'd\n-  'd = 'e\n-  'e = 'g",
    "fig": "Ellipse"
  },
  {
    "key": 26,
    "name": "Unify",
    "desc": "(Number -> 'c)\nsubsts = \n-  'a = 'd\n-  'd = 'e\n-  'e = 'c\n-  'e = 'g\n-  'e = Number",
    "fig": "Ellipse"
  },
  {
    "key": 27,
    "name": "Arg Out",
    "desc": "'c\nsubsts = \n-  'a = 'd\n-  'd = 'e\n-  'e = 'c\n-  'e = 'g\n-  'e = Number",
    "fig": "Ellipse"
  },
  {
    "key": 28,
    "name": "10 (App)",
    "desc": "Number\nsubsts = \n-  'a = 'd\n-  'c = 'g\n-  'd = 'e\n-  'e = Number\n-  'g = Number",
    "fig": "Rectangle"
  },
  {
    "key": 29,
    "name": "Inst (App)",
    "desc": "Number\nsubsts = \n-  'a = 'd\n-  'c = 'g\n-  'd = 'e\n-  'e = Number\n-  'g = Number",
    "fig": "Ellipse"
  },
  {
    "key": 30,
    "name": "MakeTuple",
    "desc": "(String * Number)\nsubsts = \n-  'a = 'd\n-  'b = 'f\n-  'c = 'g\n-  'd = 'e\n-  'e = Number\n-  'e = String\n-  'f = String\n-  'g = Number",
    "fig": "Ellipse"
  },
  {
    "key": 31,
    "name": "6 (Tuple)",
    "desc": "ERROR: Cannot unify types \"Number\" and \"String\": Type mismatch\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 32,
    "name": "Inst (Tuple)",
    "desc": "ERROR (inherited)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 33,
    "name": "1 (Let)",
    "desc": "ERROR (inherited)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 34,
    "name": "Inst (Let)",
    "desc": "ERROR (inherited)\nsubsts = [ ]",
    "fig": "Ellipse"
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
    "from": 6,
    "to": 7
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
    "from": 10,
    "to": 12
  },
  {
    "from": 11,
    "to": 12
  },
  {
    "from": 7,
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
    "from": 12,
    "to": 15
  },
  {
    "from": 15,
    "to": 16
  },
  {
    "from": 16,
    "to": 17
  },
  {
    "from": 17,
    "to": 18
  },
  {
    "from": 19,
    "to": 20
  },
  {
    "from": 20,
    "to": 21
  },
  {
    "from": 21,
    "to": 23
  },
  {
    "from": 22,
    "to": 23
  },
  {
    "from": 7,
    "to": 24
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
    "from": 23,
    "to": 26
  },
  {
    "from": 26,
    "to": 27
  },
  {
    "from": 27,
    "to": 28
  },
  {
    "from": 28,
    "to": 29
  },
  {
    "from": 18,
    "to": 30
  },
  {
    "from": 29,
    "to": 30
  },
  {
    "from": 30,
    "to": 31
  },
  {
    "from": 31,
    "to": 32
  },
  {
    "from": 32,
    "to": 33
  },
  {
    "from": 33,
    "to": 34
  }
];
    