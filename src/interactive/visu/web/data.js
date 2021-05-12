
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "App",
    "desc": "var = 1\ntype = Number\nsubsts = [ ]\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x)",
    "desc": "var = 2\ntype = ('b -> 'b)\nsubsts = [ ]\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (x)",
    "desc": "var = 4\ntype = 'b\nsubsts = [ ]\nenv = [ 'x' : (tv=3) 'b ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (42)",
    "desc": "var = 5\ntype = Number\nsubsts = [ ]\nenv = [ ]",
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
  }
];
    