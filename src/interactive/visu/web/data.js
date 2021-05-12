
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
    "name": "4 (Env)",
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
    "name": "MakeFun",
    "desc": "('a -> 'a)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 4,
    "name": "3 (Abs)",
    "desc": "('a -> 'a)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "2 (Env)",
    "desc": "('a -> 'a)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "SOURCE",
    "desc": "String\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "9 (Lit)",
    "desc": "String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "SOURCE",
    "desc": "'b\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 9,
    "name": "MakeFun",
    "desc": "(String -> 'b)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 10,
    "name": "8 (Var)",
    "desc": "('a -> 'a)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Unify",
    "desc": "(String -> 'b)\nsubsts = \n-  'a = String\n-  'b = String",
    "fig": "Ellipse"
  },
  {
    "key": 12,
    "name": "Arg Out",
    "desc": "'b\nsubsts = \n-  'a = String\n-  'b = String",
    "fig": "Ellipse"
  },
  {
    "key": 13,
    "name": "7 (App)",
    "desc": "'b\nsubsts = \n-  'a = String\n-  'b = String",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "SOURCE",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 15,
    "name": "12 (Lit)",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 16,
    "name": "SOURCE",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 17,
    "name": "MakeFun",
    "desc": "(Number -> 'c)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 18,
    "name": "11 (Var)",
    "desc": "('a -> 'a)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 19,
    "name": "Unify",
    "desc": "(Number -> 'c)\nsubsts = \n-  'a = Number\n-  'c = Number",
    "fig": "Ellipse"
  },
  {
    "key": 20,
    "name": "Arg Out",
    "desc": "'c\nsubsts = \n-  'a = Number\n-  'c = Number",
    "fig": "Ellipse"
  },
  {
    "key": 21,
    "name": "10 (App)",
    "desc": "'c\nsubsts = \n-  'a = Number\n-  'c = Number",
    "fig": "Rectangle"
  },
  {
    "key": 22,
    "name": "MakeTuple",
    "desc": "ERROR: Cannot unify types \"Number\" and \"String\": Type mismatch\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 23,
    "name": "6 (Tuple)",
    "desc": "ERROR: Cannot unify types \"Number\" and \"String\": Type mismatch\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 24,
    "name": "1 (Let)",
    "desc": "ERROR (inherited)\nsubsts = [ ]",
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
  },
  {
    "from": 2,
    "to": 3
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
    "from": 6,
    "to": 7
  },
  {
    "from": 7,
    "to": 9
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 5,
    "to": 10
  },
  {
    "from": 10,
    "to": 11
  },
  {
    "from": 9,
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
    "from": 14,
    "to": 15
  },
  {
    "from": 15,
    "to": 17
  },
  {
    "from": 16,
    "to": 17
  },
  {
    "from": 5,
    "to": 18
  },
  {
    "from": 18,
    "to": 19
  },
  {
    "from": 17,
    "to": 19
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
    "from": 13,
    "to": 22
  },
  {
    "from": 21,
    "to": 22
  },
  {
    "from": 22,
    "to": 23
  },
  {
    "from": 23,
    "to": 24
  }
];
    