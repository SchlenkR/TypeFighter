
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Fun (f) ->",
    "desc": "var = 1\ntype = ((String -> 'c) -> ('b * 'c))\ninsts = \n-  'a = 'd\n-  'a = 'e\nsubsts = \n-  'd = (String -> 'c)\n-  'e = (Number -> 'b)",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Tuple",
    "desc": "var = 3\ntype = ('b * 'c)\ninsts = \n-  'a = 'd\n-  'a = 'e\nsubsts = \n-  'd = (String -> 'c)\n-  'e = (Number -> 'b)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "App",
    "desc": "var = 4\ntype = 'b\ninsts = [ 'a = 'e ]\nsubsts = [ 'e = (Number -> 'b) ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Var (f)",
    "desc": "var = 5\ntype = 'e\ninsts = [ 'a = 'e ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Lit (42)",
    "desc": "var = 6\ntype = Number\ninsts = [ ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "App",
    "desc": "var = 7\ntype = 'c\ninsts = [ 'a = 'd ]\nsubsts = [ 'd = (String -> 'c) ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Var (f)",
    "desc": "var = 8\ntype = 'd\ninsts = [ 'a = 'd ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Lit (xxx)",
    "desc": "var = 9\ntype = String\ninsts = [ ]\nsubsts = [ ]",
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
    "to": 5
  },
  {
    "from": 2,
    "to": 3
  },
  {
    "from": 2,
    "to": 4
  },
  {
    "from": 5,
    "to": 6
  },
  {
    "from": 5,
    "to": 7
  }
];
    