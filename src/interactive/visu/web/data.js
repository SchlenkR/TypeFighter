
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Fun (f) ->",
    "desc": "var = 1\ntype = ('a -> 'b)\ninsts = [ 'c = 'd ]\nsubsts = [ 'd = (Number -> 'b) ]",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "App",
    "desc": "var = 3\ntype = 'b\ninsts = [ 'c = 'd ]\nsubsts = [ 'd = (Number -> 'b) ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (f)",
    "desc": "var = 4\ntype = 'd\ninsts = [ 'c = 'd ]\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (42)",
    "desc": "var = 5\ntype = Number\ninsts = [ ]\nsubsts = [ ]",
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
    