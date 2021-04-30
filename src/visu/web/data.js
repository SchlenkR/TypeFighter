
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let x",
    "desc": "type = String",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Record",
    "desc": "fields = { a; b }\ntype = { a: Number; b: String }",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Lit (5)",
    "desc": "type = Number",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Lit (hello)",
    "desc": "type = String",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Prop b",
    "desc": "type = String",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var (x)",
    "desc": "type = { a: Number; b: String }",
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
  }
];
    