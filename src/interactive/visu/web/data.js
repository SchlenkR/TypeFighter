
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Fun (f) ->",
    "desc": "var = 1\ntype = ((Number -> 'b) -> 'b)\nsubsts = \n-  'a = 'c\n-  'c = (Number -> 'b)\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "App",
    "desc": "var = 3\ntype = 'b\nsubsts = \n-  'a = 'c\n-  'c = (Number -> 'b)\nenv = [ 'f' : (var=2;parent=1) 'a ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (f)",
    "desc": "var = 4\ntype = 'c\nsubsts = [ 'a = 'c ]\nenv = [ 'f' : (var=2;parent=1) 'a ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (42)",
    "desc": "var = 5\ntype = Number\nsubsts = [ ]\nenv = [ 'f' : (var=2;parent=1) 'a ]",
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
  }
];
    