
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
    "name": "8 (Env)",
    "desc": "'a\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "9 (Var)",
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
    "name": "7 (Abs)",
    "desc": "('a -> 'a)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "SOURCE",
    "desc": "'b\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 6,
    "name": "MakeFun",
    "desc": "(('a -> 'a) -> 'b)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "SOURCE",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 8,
    "name": "3 (Env)",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "SOURCE",
    "desc": "String\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 10,
    "name": "6 (Lit)",
    "desc": "String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "SOURCE",
    "desc": "'d\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 12,
    "name": "MakeFun",
    "desc": "(String -> 'd)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 13,
    "name": "5 (Var)",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "Unify",
    "desc": "(String -> 'd)\nsubsts = [ 'c = (String -> 'd) ]",
    "fig": "Ellipse"
  },
  {
    "key": 15,
    "name": "Arg Out",
    "desc": "'d\nsubsts = [ 'c = (String -> 'd) ]",
    "fig": "Ellipse"
  },
  {
    "key": 16,
    "name": "4 (App)",
    "desc": "'d\nsubsts = [ 'c = (String -> 'd) ]",
    "fig": "Rectangle"
  },
  {
    "key": 17,
    "name": "MakeFun",
    "desc": "('c -> 'd)\nsubsts = [ 'c = (String -> 'd) ]",
    "fig": "Ellipse"
  },
  {
    "key": 18,
    "name": "2 (Abs)",
    "desc": "('c -> 'd)\nsubsts = [ 'c = (String -> 'd) ]",
    "fig": "Rectangle"
  },
  {
    "key": 19,
    "name": "Unify",
    "desc": "(('b -> 'b) -> 'b)\nsubsts = \n-  'a = 'd\n-  'a = String\n-  'c = (String -> 'd)\n-  'd = 'b",
    "fig": "Ellipse"
  },
  {
    "key": 20,
    "name": "Arg Out",
    "desc": "'b\nsubsts = \n-  'a = 'd\n-  'a = String\n-  'c = (String -> 'd)\n-  'd = 'b",
    "fig": "Ellipse"
  },
  {
    "key": 21,
    "name": "1 (App)",
    "desc": "'b\nsubsts = \n-  'a = 'd\n-  'a = String\n-  'c = (String -> 'd)\n-  'd = 'b",
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
    "to": 6
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
    "from": 8,
    "to": 13
  },
  {
    "from": 13,
    "to": 14
  },
  {
    "from": 12,
    "to": 14
  },
  {
    "from": 14,
    "to": 15
  },
  {
    "from": 15,
    "to": 16
  },
  {
    "from": 8,
    "to": 17
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
    "from": 18,
    "to": 19
  },
  {
    "from": 6,
    "to": 19
  },
  {
    "from": 19,
    "to": 20
  },
  {
    "from": 20,
    "to": 21
  }
];
    