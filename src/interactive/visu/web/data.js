
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "App",
    "desc": "var = 1\ntype = Seq<'a>\nsubsts = \n-  'a = Number\n-  'a = String",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "App",
    "desc": "var = 2\ntype = (Seq<'a> -> Seq<'a>)\nsubsts = [ 'a = Number ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (cons)",
    "desc": "var = 3\ntype = ('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (1)",
    "desc": "var = 4\ntype = Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "App",
    "desc": "var = 5\ntype = Seq<'a>\nsubsts = \n-  'a = Number\n-  'a = String",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "App",
    "desc": "var = 6\ntype = (Seq<'a> -> Seq<'a>)\nsubsts = [ 'a = Number ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Var (cons)",
    "desc": "var = 7\ntype = ('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Lit (2)",
    "desc": "var = 8\ntype = Number\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "App",
    "desc": "var = 9\ntype = Seq<'a>\nsubsts = [ 'a = String ]",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "App",
    "desc": "var = 10\ntype = (Seq<'a> -> Seq<'a>)\nsubsts = [ 'a = String ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Var (cons)",
    "desc": "var = 11\ntype = ('a -> (Seq<'a> -> Seq<'a>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Lit (xxx)",
    "desc": "var = 12\ntype = String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 12,
    "name": "Var (emptyList)",
    "desc": "var = 13\ntype = Seq<'a>\nsubsts = [ ]",
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
    "to": 4
  },
  {
    "from": 1,
    "to": 2
  },
  {
    "from": 1,
    "to": 3
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 4,
    "to": 8
  },
  {
    "from": 5,
    "to": 6
  },
  {
    "from": 5,
    "to": 7
  },
  {
    "from": 8,
    "to": 9
  },
  {
    "from": 8,
    "to": 12
  },
  {
    "from": 9,
    "to": 10
  },
  {
    "from": 9,
    "to": 11
  }
];
    