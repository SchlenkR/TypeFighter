
window.layout = "tree";
window.nodeDataArray = [
  {
    "key": 17,
    "name": "Let play = ...",
    "varNum": "17",
    "additionalInfo": "IDENT = play   TVAR = tv_0   TYP = (Number -\u003E String)",
    "exprTyp": "String",
    "env": "",
    "fig": "Rectangle"
  },
  {
    "key": 13,
    "name": "Fun number -\u003E ...",
    "varNum": "13",
    "additionalInfo": "IDENT = number   TVAR = tv_1   TYP = Number",
    "exprTyp": "(Number -\u003E String)",
    "env": "",
    "fig": "Rectangle"
  },
  {
    "key": 12,
    "name": "App",
    "varNum": "12",
    "additionalInfo": "",
    "exprTyp": "String",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 10,
    "name": "App",
    "varNum": "10",
    "additionalInfo": "",
    "exprTyp": "(String -\u003E String)",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 8,
    "name": "App",
    "varNum": "8",
    "additionalInfo": "",
    "exprTyp": "(String -\u003E (String -\u003E String))",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 2,
    "name": "Var \u0022if\u0022 ",
    "varNum": "2",
    "additionalInfo": "",
    "exprTyp": "(Bool -\u003E (String -\u003E (String -\u003E String)))",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 7,
    "name": "App",
    "varNum": "7",
    "additionalInfo": "",
    "exprTyp": "Bool",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 5,
    "name": "App",
    "varNum": "5",
    "additionalInfo": "",
    "exprTyp": "(Number -\u003E Bool)",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 3,
    "name": "Var \u0022equals\u0022 ",
    "varNum": "3",
    "additionalInfo": "",
    "exprTyp": "(Number -\u003E (Number -\u003E Bool))",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 4,
    "name": "Var \u0022number\u0022 ",
    "varNum": "4",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 6,
    "name": "Lit (42) ",
    "varNum": "6",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 9,
    "name": "Lit (\u0027win\u0027) ",
    "varNum": "9",
    "additionalInfo": "",
    "exprTyp": "String",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 11,
    "name": "Lit (\u0027lose\u0027) ",
    "varNum": "11",
    "additionalInfo": "",
    "exprTyp": "String",
    "env": "number: tv_1 (Number)",
    "fig": "Rectangle"
  },
  {
    "key": 16,
    "name": "App",
    "varNum": "16",
    "additionalInfo": "",
    "exprTyp": "String",
    "env": "play: tv_0 ((Number -\u003E String))",
    "fig": "Rectangle"
  },
  {
    "key": 14,
    "name": "Var \u0022play\u0022 ",
    "varNum": "14",
    "additionalInfo": "",
    "exprTyp": "(Number -\u003E String)",
    "env": "play: tv_0 ((Number -\u003E String))",
    "fig": "Rectangle"
  },
  {
    "key": 15,
    "name": "Lit (99) ",
    "varNum": "15",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": "play: tv_0 ((Number -\u003E String))",
    "fig": "Rectangle"
  }
];
window.linkDataArray = [
  {
    "from": 17,
    "to": 13
  },
  {
    "from": 17,
    "to": 16
  },
  {
    "from": 13,
    "to": 12
  },
  {
    "from": 12,
    "to": 10
  },
  {
    "from": 12,
    "to": 11
  },
  {
    "from": 10,
    "to": 8
  },
  {
    "from": 10,
    "to": 9
  },
  {
    "from": 8,
    "to": 2
  },
  {
    "from": 8,
    "to": 7
  },
  {
    "from": 7,
    "to": 5
  },
  {
    "from": 7,
    "to": 6
  },
  {
    "from": 5,
    "to": 3
  },
  {
    "from": 5,
    "to": 4
  },
  {
    "from": 16,
    "to": 14
  },
  {
    "from": 16,
    "to": 15
  }
];
        