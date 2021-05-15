
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "App",
    "desc": "var = 1\ntype = ERROR (inherited)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (id)",
    "desc": "var = 2\ntype = ERROR (inherited)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Tuple",
    "desc": "var = 4\ntype = ERROR: Cannot unify types \"Number\" and \"String\": Type mismatch\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "App",
    "desc": "var = 5\ntype = 'd\nsubsts = [ 'c = (String -> 'd) ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var (id)",
    "desc": "var = 6\ntype = 'c\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Lit (Hello World)",
    "desc": "var = 7\ntype = String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "App",
    "desc": "var = 8\ntype = 'e\nsubsts = [ 'c = (Number -> 'e) ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Var (id)",
    "desc": "var = 9\ntype = 'c\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Lit (42)",
    "desc": "var = 10\ntype = Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Fun (x)",
    "desc": "var = 11\ntype = ('a -> 'a)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Var (x)",
    "desc": "var = 13\ntype = 'a\nsubsts = [ ]",
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
    "to": 9
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
    "from": 2,
    "to": 6
  },
  {
    "from": 3,
    "to": 4
  },
  {
    "from": 3,
    "to": 5
  },
  {
    "from": 6,
    "to": 7
  },
  {
    "from": 6,
    "to": 8
  },
  {
    "from": 9,
    "to": 10
  }
];
    