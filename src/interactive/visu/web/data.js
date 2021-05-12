
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let x",
    "desc": "var = 1\ntype = Seq<'b>\nsubsts = \n-  'b = Number\n-  'c = Number",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Lit (10)",
    "desc": "var = 3\ntype = Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "App",
    "desc": "var = 4\ntype = Seq<'b>\nsubsts = \n-  'b = Number\n-  'c = Number",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "App",
    "desc": "var = 5\ntype = (('a -> 'b) -> Seq<'b>)\nsubsts = [ 'a = Number ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var (map)",
    "desc": "var = 6\ntype = (Seq<'a> -> (('a -> 'b) -> Seq<'b>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var (Numbers)",
    "desc": "var = 7\ntype = Seq<Number>\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Fun (number)",
    "desc": "var = 8\ntype = ('c -> Number)\nsubsts = [ 'c = Number ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "App",
    "desc": "var = 10\ntype = Number\nsubsts = [ 'c = Number ]",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "App",
    "desc": "var = 11\ntype = (Number -> Number)\nsubsts = [ 'c = Number ]",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Var (add)",
    "desc": "var = 12\ntype = (Number -> (Number -> Number))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Var (number)",
    "desc": "var = 13\ntype = 'c\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Var (x)",
    "desc": "var = 14\ntype = Number\nsubsts = [ ]",
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
    "from": 7,
    "to": 8
  },
  {
    "from": 7,
    "to": 11
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 8,
    "to": 10
  }
];
    