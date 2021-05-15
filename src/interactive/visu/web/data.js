
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let id = ...",
    "desc": "var = 1\ntype = (String * Number)\nsubsts = \n-  'a = 'd\n-  'b = String\n-  'c = Number\n-  'd = 'f\n-  'e = String\n-  'f = Number\n-  'f = String",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x) ->",
    "desc": "var = 3\ntype = ('d -> 'd)\nsubsts = [ 'a = 'd ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (x)",
    "desc": "var = 5\ntype = 'd\nsubsts = [ 'a = 'd ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Tuple",
    "desc": "var = 6\ntype = (String * Number)\nsubsts = \n-  'a = 'd\n-  'b = String\n-  'c = Number\n-  'd = 'f\n-  'e = 'f\n-  'e = String\n-  'f = Number",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "App",
    "desc": "var = 7\ntype = String\nsubsts = \n-  'a = 'd\n-  'b = String\n-  'd = 'e\n-  'e = String",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var (id)",
    "desc": "var = 8\ntype = ('e -> 'e)\nsubsts = \n-  'a = 'd\n-  'd = 'e",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Lit (Hello World)",
    "desc": "var = 9\ntype = String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "App",
    "desc": "var = 10\ntype = Number\nsubsts = \n-  'a = 'd\n-  'c = Number\n-  'd = 'f\n-  'f = Number",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Var (id)",
    "desc": "var = 11\ntype = ('f -> 'f)\nsubsts = \n-  'a = 'd\n-  'd = 'f",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Lit (42)",
    "desc": "var = 12\ntype = Number\nsubsts = [ ]",
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
    "to": 7
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 4,
    "to": 6
  },
  {
    "from": 7,
    "to": 8
  },
  {
    "from": 7,
    "to": 9
  }
];
    