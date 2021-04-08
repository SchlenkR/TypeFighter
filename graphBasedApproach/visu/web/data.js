window.nodeDataArray = [
  {
    "desc": ": 0",
    "fig": "Rectangle",
    "key": 0,
    "name": "let f = [e1: 1] in [e2: 4]"
  },
  {
    "desc": ": 1",
    "fig": "Rectangle",
    "key": 1,
    "name": "fun (x: 2) -> [e: 3]"
  },
  {
    "desc": ": 3",
    "fig": "Rectangle",
    "key": 2,
    "name": "Var (x: 2)"
  },
  {
    "desc": ": 4",
    "fig": "Rectangle",
    "key": 3,
    "name": "let res1 = [e1: 5] in [e2: 8]"
  },
  {
    "desc": ": 5",
    "fig": "Rectangle",
    "key": 4,
    "name": "App"
  },
  {
    "desc": ": 6",
    "fig": "Rectangle",
    "key": 5,
    "name": "Var (f: 1)"
  },
  {
    "desc": ": 7",
    "fig": "Rectangle",
    "key": 6,
    "name": "Lit (99:Int)"
  },
  {
    "desc": ": 8",
    "fig": "Rectangle",
    "key": 7,
    "name": "let res2 = [e1: 9] in [e2: 12]"
  },
  {
    "desc": ": 9",
    "fig": "Rectangle",
    "key": 8,
    "name": "App"
  },
  {
    "desc": ": 10",
    "fig": "Rectangle",
    "key": 9,
    "name": "Var (f: 1)"
  },
  {
    "desc": ": 11",
    "fig": "Rectangle",
    "key": 10,
    "name": "Lit (HelloWorld:String)"
  },
  {
    "desc": ": 12",
    "fig": "Rectangle",
    "key": 11,
    "name": "Var (res2: 9)"
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
