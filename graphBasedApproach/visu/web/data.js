
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 0,
    "name": "let f = {e1 : 3} in {e2 : 6}",
    "desc": "var = 1\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 1,
    "name": "fun {x : 4} -> {e : 5}",
    "desc": "var = 3\nenv = [ ]",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var {x : 4}",
    "desc": "var = 5\nenv = [ {x : 4} ]",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "let res1 = {e1 : 8} in {e2 : 11}",
    "desc": "var = 6\nenv = [ {f : 2} ]",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "App",
    "desc": "var = 8\nenv = [ {f : 2} ]",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "Var {f : 2}",
    "desc": "var = 9\nenv = [ {f : 2} ]",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Lit (99: double)",
    "desc": "var = 10\nenv = [ {f : 2} ]",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "let res2 = {e1 : 13} in {e2 : 16}",
    "desc": "var = 11\nenv = [\n-  {f : 2}\n-  {res1 : 7} ]",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "App",
    "desc": "var = 13\nenv = [\n-  {f : 2}\n-  {res1 : 7} ]",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Var {f : 2}",
    "desc": "var = 14\nenv = [\n-  {f : 2}\n-  {res1 : 7} ]",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "Lit (HelloWorld: string)",
    "desc": "var = 15\nenv = [\n-  {f : 2}\n-  {res1 : 7} ]",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Var {res2 : 12}",
    "desc": "var = 16\nenv = [\n-  {f : 2}\n-  {res1 : 7}\n-  {res2 : 12} ]",
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
