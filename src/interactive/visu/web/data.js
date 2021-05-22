
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let f = ...",
    "desc": "var = 1\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x) ->",
    "desc": "var = 3\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (x)",
    "desc": "var = 5\nenv = [ 'x' : ??? | tv=4 ref=- ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Let rec = ...",
    "desc": "var = 6\nenv = [ 'f' : ??? | tv=2 ref=3 ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Record",
    "desc": "fields = { myFunc }\nvar = 8\nenv = [ 'f' : ??? | tv=2 ref=3 ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var (f)",
    "desc": "var = 9\nenv = [ 'f' : ??? | tv=2 ref=3 ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "App",
    "desc": "var = 10\nenv = \n-  'f' : ??? | tv=2 ref=3\n-  'rec' : ??? | tv=7 ref=8",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Prop myFunc",
    "desc": "var = 11\nenv = \n-  'f' : ??? | tv=2 ref=3\n-  'rec' : ??? | tv=7 ref=8",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Var (rec)",
    "desc": "var = 12\nenv = \n-  'f' : ??? | tv=2 ref=3\n-  'rec' : ??? | tv=7 ref=8",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Lit (42)",
    "desc": "var = 13\nenv = \n-  'f' : ??? | tv=2 ref=3\n-  'rec' : ??? | tv=7 ref=8",
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
    "to": 6
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
    "from": 6,
    "to": 9
  },
  {
    "from": 7,
    "to": 8
  }
];
    