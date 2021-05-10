
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let id",
    "desc": "var = 1\ntype = { field1: String; field2: Number }",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x)",
    "desc": "var = 3\ntype = ('a -> 'a)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (x)",
    "desc": "var = 5\ntype = 'a",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Let add",
    "desc": "var = 6\ntype = { field1: String; field2: Number }",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Fun (a)",
    "desc": "var = 8\ntype = ('b -> ('c -> { field1: 'b; field2: 'c }))",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Fun (b)",
    "desc": "var = 10\ntype = ('c -> { field1: 'b; field2: 'c })",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Record",
    "desc": "fields = { field1; field2 }\nvar = 12\ntype = { field1: 'b; field2: 'c }",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Var (a)",
    "desc": "var = 13\ntype = 'b",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Var (b)",
    "desc": "var = 14\ntype = 'c",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "App",
    "desc": "var = 15\ntype = { field1: String; field2: Number }",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "App",
    "desc": "var = 16\ntype = ('c -> { field1: String; field2: 'c })",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Var (add)",
    "desc": "var = 17\ntype = ('b -> ('c -> { field1: 'b; field2: 'c }))",
    "fig": "Rectangle"
  },
  {
    "key": 12,
    "name": "Lit (Hello)",
    "desc": "var = 18\ntype = String",
    "fig": "Rectangle"
  },
  {
    "key": 13,
    "name": "App",
    "desc": "var = 19\ntype = Number",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "Var (id)",
    "desc": "var = 20\ntype = ('a -> 'a)",
    "fig": "Rectangle"
  },
  {
    "key": 15,
    "name": "Lit (42)",
    "desc": "var = 21\ntype = Number",
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
    "to": 9
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 5,
    "to": 6
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
  },
  {
    "from": 9,
    "to": 13
  },
  {
    "from": 10,
    "to": 11
  },
  {
    "from": 10,
    "to": 12
  },
  {
    "from": 13,
    "to": 14
  },
  {
    "from": 13,
    "to": 15
  }
];
    