
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let myValue = ...",
    "desc": "var = 1\ntype = Number\nsubsts = \n-  'a = 'c\n-  'b = Number\n-  'c = 'd\n-  'd = Number\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Lit (yyyy)",
    "desc": "var = 3\ntype = String\nsubsts = [ ]\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Let id = ...",
    "desc": "var = 4\ntype = Number\nsubsts = \n-  'a = 'c\n-  'b = Number\n-  'c = 'd\n-  'd = Number\nenv = [ 'myValue' : (tyvar=2) String ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Fun (x) ->",
    "desc": "var = 6\ntype = ('c -> 'c)\nsubsts = [ 'a = 'c ]\nenv = [ 'myValue' : (tyvar=2) String ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var (x)",
    "desc": "var = 8\ntype = 'c\nsubsts = [ 'a = 'c ]\nenv = \n-  'myValue' : (tyvar=2) String\n-  'x' : (tyvar=7) 'a",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Let res = ...",
    "desc": "var = 9\ntype = Number\nsubsts = \n-  'a = 'c\n-  'b = Number\n-  'c = 'd\n-  'd = Number\nenv = \n-  'id' : (tyvar=5) ('c -> 'c)\n-  'myValue' : (tyvar=2) String",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "App",
    "desc": "var = 11\ntype = Number\nsubsts = \n-  'a = 'c\n-  'b = Number\n-  'c = 'd\n-  'd = Number\nenv = \n-  'id' : (tyvar=5) ('c -> 'c)\n-  'myValue' : (tyvar=2) String",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Var (id)",
    "desc": "var = 12\ntype = ('d -> 'd)\nsubsts = \n-  'a = 'c\n-  'c = 'd\nenv = \n-  'id' : (tyvar=5) ('c -> 'c)\n-  'myValue' : (tyvar=2) String",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Lit (23,9)",
    "desc": "var = 13\ntype = Number\nsubsts = [ ]\nenv = \n-  'id' : (tyvar=5) ('c -> 'c)\n-  'myValue' : (tyvar=2) String",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Var (res)",
    "desc": "var = 14\ntype = Number\nsubsts = \n-  'a = 'c\n-  'b = Number\n-  'c = 'd\n-  'd = Number\nenv = \n-  'id' : (tyvar=5) ('c -> 'c)\n-  'myValue' : (tyvar=2) String\n-  'res' : (tyvar=10) Number",
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
    "to": 2
  },
  {
    "from": 2,
    "to": 3
  },
  {
    "from": 2,
    "to": 5
  },
  {
    "from": 3,
    "to": 4
  },
  {
    "from": 5,
    "to": 6
  },
  {
    "from": 5,
    "to": 9
  },
  {
    "from": 6,
    "to": 7
  },
  {
    "from": 6,
    "to": 8
  }
];
    