
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "App",
    "desc": "var = 1\ntype = (Seq<'c> -> Seq<String>)\nsubsts = \n-  'a = 'c\n-  'b = String\n-  'd = String\n-  'e = (Seq<'a> -> Seq<'b>)",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Var (mapp)",
    "desc": "var = 2\ntype = (('a -> 'b) -> (Seq<'a> -> Seq<'b>))\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Fun (x)",
    "desc": "var = 3\ntype = ('c -> String)\nsubsts = \n-  'a = 'c\n-  'd = String",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "App",
    "desc": "var = 5\ntype = String\nsubsts = \n-  'a = 'c\n-  'd = String",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var (tostring)",
    "desc": "var = 6\ntype = ('a -> String)\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var (x)",
    "desc": "var = 7\ntype = 'c\nsubsts = [ ]",
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
    "from": 3,
    "to": 4
  },
  {
    "from": 3,
    "to": 5
  }
];
    