window.nodeDataArray = [
  {
    "desc": "var = 0\nenv = [ ]",
    "fig": "Rectangle",
    "key": 0,
    "name": "let f = {e1: 2} in {e2: 5}"
  },
  {
    "desc": "var = 2\nenv = [ ]",
    "fig": "Rectangle",
    "key": 1,
    "name": "fun {x: 3} -> {e: 4}"
  },
  {
    "desc": "var = 4\nenv = [ {x: 3} ]",
    "fig": "Rectangle",
    "key": 2,
    "name": "Var {x: 3}"
  },
  {
    "desc": "var = 5\nenv = [ {f: 1} ]",
    "fig": "Rectangle",
    "key": 3,
    "name": "let res1 = {e1: 7} in {e2: 10}"
  },
  {
    "desc": "var = 7\nenv = [ {f: 1} ]",
    "fig": "Rectangle",
    "key": 4,
    "name": "App"
  },
  {
    "desc": "var = 8\nenv = [ {f: 1} ]",
    "fig": "Rectangle",
    "key": 5,
    "name": "Var {f: 1}"
  },
  {
    "desc": "var = 9\nenv = [ {f: 1} ]",
    "fig": "Rectangle",
    "key": 6,
    "name": "Lit (99: Int)"
  },
  {
    "desc": "var = 10\nenv = [\n-  {f: 1}\n-  {res1: 6} ]",
    "fig": "Rectangle",
    "key": 7,
    "name": "let res2 = {e1: 12} in {e2: 15}"
  },
  {
    "desc": "var = 12\nenv = [\n-  {f: 1}\n-  {res1: 6} ]",
    "fig": "Rectangle",
    "key": 8,
    "name": "App"
  },
  {
    "desc": "var = 13\nenv = [\n-  {f: 1}\n-  {res1: 6} ]",
    "fig": "Rectangle",
    "key": 9,
    "name": "Var {f: 1}"
  },
  {
    "desc": "var = 14\nenv = [\n-  {f: 1}\n-  {res1: 6} ]",
    "fig": "Rectangle",
    "key": 10,
    "name": "Lit (HelloWorld: String)"
  },
  {
    "desc": "var = 15\nenv = [\n-  {f: 1}\n-  {res1: 6}\n-  {res2: 11} ]",
    "fig": "Rectangle",
    "key": 11,
    "name": "Var {res2: 11}"
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
