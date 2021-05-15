
window.layout = "graph";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "SOURCE",
    "desc": "Seq<'a>\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 1,
    "name": "13 (Var)",
    "desc": "Seq<'a>\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "SOURCE",
    "desc": "'b\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 3,
    "name": "MakeFun",
    "desc": "(Seq<'a> -> 'b)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 4,
    "name": "SOURCE",
    "desc": "String\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 5,
    "name": "12 (Lit)",
    "desc": "String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "SOURCE",
    "desc": "'c\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 7,
    "name": "MakeFun",
    "desc": "(String -> 'c)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 8,
    "name": "SOURCE",
    "desc": "('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 9,
    "name": "11 (Var)",
    "desc": "('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Unify",
    "desc": "(String -> (Seq<String> -> Seq<String>))\nsubsts = \n-  'a = String\n-  'c = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 11,
    "name": "Arg Out",
    "desc": "(Seq<String> -> Seq<String>)\nsubsts = \n-  'a = String\n-  'c = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 12,
    "name": "10 (App)",
    "desc": "(Seq<String> -> Seq<String>)\nsubsts = \n-  'a = String\n-  'c = (Seq<'a> -> Seq<'a>)",
    "fig": "Rectangle"
  },
  {
    "key": 13,
    "name": "Unify",
    "desc": "(Seq<String> -> Seq<String>)\nsubsts = \n-  'a = String\n-  'b = Seq<String>\n-  'c = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 14,
    "name": "Arg Out",
    "desc": "Seq<String>\nsubsts = \n-  'a = String\n-  'b = Seq<String>\n-  'c = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 15,
    "name": "9 (App)",
    "desc": "Seq<String>\nsubsts = \n-  'a = String\n-  'b = Seq<String>\n-  'c = (Seq<'a> -> Seq<'a>)",
    "fig": "Rectangle"
  },
  {
    "key": 16,
    "name": "SOURCE",
    "desc": "'d\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 17,
    "name": "MakeFun",
    "desc": "(Seq<String> -> 'd)\nsubsts = \n-  'a = String\n-  'b = Seq<String>\n-  'c = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 18,
    "name": "SOURCE",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 19,
    "name": "8 (Lit)",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 20,
    "name": "SOURCE",
    "desc": "'e\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 21,
    "name": "MakeFun",
    "desc": "(Number -> 'e)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 22,
    "name": "SOURCE",
    "desc": "('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 23,
    "name": "7 (Var)",
    "desc": "('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 24,
    "name": "Unify",
    "desc": "(Number -> (Seq<Number> -> Seq<Number>))\nsubsts = \n-  'a = Number\n-  'e = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 25,
    "name": "Arg Out",
    "desc": "(Seq<Number> -> Seq<Number>)\nsubsts = \n-  'a = Number\n-  'e = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 26,
    "name": "6 (App)",
    "desc": "(Seq<Number> -> Seq<Number>)\nsubsts = \n-  'a = Number\n-  'e = (Seq<'a> -> Seq<'a>)",
    "fig": "Rectangle"
  },
  {
    "key": 27,
    "name": "SOURCE",
    "desc": "'f\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 28,
    "name": "SOURCE",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 29,
    "name": "4 (Lit)",
    "desc": "Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 30,
    "name": "SOURCE",
    "desc": "'g\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 31,
    "name": "MakeFun",
    "desc": "(Number -> 'g)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 32,
    "name": "SOURCE",
    "desc": "('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 33,
    "name": "3 (Var)",
    "desc": "('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 34,
    "name": "Unify",
    "desc": "(Number -> (Seq<Number> -> Seq<Number>))\nsubsts = \n-  'a = Number\n-  'g = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 35,
    "name": "Arg Out",
    "desc": "(Seq<Number> -> Seq<Number>)\nsubsts = \n-  'a = Number\n-  'g = (Seq<'a> -> Seq<'a>)",
    "fig": "Ellipse"
  },
  {
    "key": 36,
    "name": "2 (App)",
    "desc": "(Seq<Number> -> Seq<Number>)\nsubsts = \n-  'a = Number\n-  'g = (Seq<'a> -> Seq<'a>)",
    "fig": "Rectangle"
  },
  {
    "key": 37,
    "name": "Unify",
    "desc": "ERROR: Cannot unify types \"Number\" and \"String\": Type mismatch\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 38,
    "name": "Arg Out",
    "desc": "ERROR: Cannot unify types \"Number\" and \"String\": Type mismatch\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 39,
    "name": "5 (App)",
    "desc": "ERROR: Cannot unify types \"Number\" and \"String\": Type mismatch\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 40,
    "name": "MakeFun",
    "desc": "ERROR (inherited)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 41,
    "name": "Unify",
    "desc": "ERROR (inherited)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 42,
    "name": "Arg Out",
    "desc": "ERROR (inherited)\nsubsts = [ ]",
    "fig": "Ellipse"
  },
  {
    "key": 43,
    "name": "1 (App)",
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
    "to": 7
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
    "from": 7,
    "to": 10
  },
  {
    "from": 10,
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
    "from": 3,
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
    "from": 15,
    "to": 17
  },
  {
    "from": 16,
    "to": 17
  },
  {
    "from": 18,
    "to": 19
  },
  {
    "from": 19,
    "to": 21
  },
  {
    "from": 20,
    "to": 21
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
    "from": 21,
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
    "from": 28,
    "to": 29
  },
  {
    "from": 29,
    "to": 31
  },
  {
    "from": 30,
    "to": 31
  },
  {
    "from": 32,
    "to": 33
  },
  {
    "from": 33,
    "to": 34
  },
  {
    "from": 31,
    "to": 34
  },
  {
    "from": 34,
    "to": 35
  },
  {
    "from": 35,
    "to": 36
  },
  {
    "from": 26,
    "to": 37
  },
  {
    "from": 17,
    "to": 37
  },
  {
    "from": 37,
    "to": 38
  },
  {
    "from": 38,
    "to": 39
  },
  {
    "from": 39,
    "to": 40
  },
  {
    "from": 27,
    "to": 40
  },
  {
    "from": 36,
    "to": 41
  },
  {
    "from": 40,
    "to": 41
  },
  {
    "from": 41,
    "to": 42
  },
  {
    "from": 42,
    "to": 43
  }
];
    