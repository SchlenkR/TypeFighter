
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let id",
    "desc": "var = 1\ntype = ({ whatever: String } * { whatever: Number })",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x)",
    "desc": "var = 3\ntype = ('a -> { whatever: 'a })",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Record",
    "desc": "fields = { whatever }\nvar = 5\ntype = { whatever: 'a }",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Var (x)",
    "desc": "var = 6\ntype = 'a",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Tuple",
    "desc": "var = 7\ntype = ({ whatever: String } * { whatever: Number })",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "App",
    "desc": "var = 8\ntype = { whatever: String }",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Var (id)",
    "desc": "var = 9\ntype = ('a -> { whatever: 'a })",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "Lit (Hello World)",
    "desc": "var = 10\ntype = String",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "App",
    "desc": "var = 11\ntype = { whatever: Number }",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Var (id)",
    "desc": "var = 12\ntype = ('a -> { whatever: 'a })",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Lit (42)",
    "desc": "var = 13\ntype = Number",
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
    "from": 2,
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
    "to": 10
  }
];
    