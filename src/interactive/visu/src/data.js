
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "Let id = ...",
    "desc": "var = 1\ntype = (String * Number)\nsubsts = \n-  \u0027a = \u0027d\n-  \u0027b = String\n-  \u0027c = Number\n-  \u0027d = \u0027e\n-  \u0027e = \u0027g\n-  \u0027f = String\n-  \u0027g = Number\n-  \u0027g = String",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "Fun (x) -\u003E",
    "desc": "var = 3\ntype = (\u0027d -\u003E \u0027d)\nsubsts = [ \u0027a = \u0027d ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var (x)",
    "desc": "var = 5\ntype = \u0027a\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Tuple",
    "desc": "var = 6\ntype = (String * Number)\nsubsts = \n-  \u0027a = \u0027d\n-  \u0027b = String\n-  \u0027c = Number\n-  \u0027d = \u0027e\n-  \u0027e = \u0027g\n-  \u0027f = \u0027g\n-  \u0027f = String\n-  \u0027g = Number",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "App",
    "desc": "var = 7\ntype = String\nsubsts = \n-  \u0027a = \u0027d\n-  \u0027b = String\n-  \u0027d = \u0027e\n-  \u0027e = \u0027f\n-  \u0027f = String",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var (id)",
    "desc": "var = 8\ntype = (\u0027e -\u003E \u0027e)\nsubsts = \n-  \u0027a = \u0027d\n-  \u0027d = \u0027e",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Lit (Hello World)",
    "desc": "var = 9\ntype = String\nsubsts = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "App",
    "desc": "var = 10\ntype = Number\nsubsts = \n-  \u0027a = \u0027d\n-  \u0027c = Number\n-  \u0027d = \u0027e\n-  \u0027e = \u0027g\n-  \u0027g = Number",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "Var (id)",
    "desc": "var = 11\ntype = (\u0027e -\u003E \u0027e)\nsubsts = \n-  \u0027a = \u0027d\n-  \u0027d = \u0027e",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Lit (42)",
    "desc": "var = 12\ntype = Number\nsubsts = [ ]",
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
    "to": 7
  },
  {
    "from": 4,
    "to": 5
  },
  {
    "from": 4,
    "to": 6
  },
  {
    "from": 7,
    "to": 8
  },
  {
    "from": 7,
    "to": 9
  }
];
    