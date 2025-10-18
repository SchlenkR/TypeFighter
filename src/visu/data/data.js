
window.nodeDataArray = [
  {
    "key": 10,
    "name": "LET",
    "code": "x = ...",
    "varNum": "10",
    "additionalInfo": "IDENT = x   TVAR = tv_0   TYP = Number",
    "exprTyp": "Number",
    "env": []
  },
  {
    "key": 1,
    "name": "LIT",
    "code": "10",
    "varNum": "1",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": []
  },
  {
    "key": 9,
    "name": "LET",
    "code": "y = ...",
    "varNum": "9",
    "additionalInfo": "IDENT = y   TVAR = tv_2   TYP = Number",
    "exprTyp": "Number",
    "env": [
      {
        "ident": "x",
        "varNum": 0,
        "solvedTyp": "Number"
      }
    ]
  },
  {
    "key": 3,
    "name": "LIT",
    "code": "20",
    "varNum": "3",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": [
      {
        "ident": "x",
        "varNum": 0,
        "solvedTyp": "Number"
      }
    ]
  },
  {
    "key": 8,
    "name": "APP",
    "code": "",
    "varNum": "8",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": [
      {
        "ident": "x",
        "varNum": 0,
        "solvedTyp": "Number"
      },
      {
        "ident": "y",
        "varNum": 2,
        "solvedTyp": "Number"
      }
    ]
  },
  {
    "key": 6,
    "name": "APP",
    "code": "",
    "varNum": "6",
    "additionalInfo": "",
    "exprTyp": "(Number -\u003E Number)",
    "env": [
      {
        "ident": "x",
        "varNum": 0,
        "solvedTyp": "Number"
      },
      {
        "ident": "y",
        "varNum": 2,
        "solvedTyp": "Number"
      }
    ]
  },
  {
    "key": 4,
    "name": "VAR",
    "code": "add",
    "varNum": "4",
    "additionalInfo": "",
    "exprTyp": "(Number -\u003E (Number -\u003E Number))",
    "env": [
      {
        "ident": "x",
        "varNum": 0,
        "solvedTyp": "Number"
      },
      {
        "ident": "y",
        "varNum": 2,
        "solvedTyp": "Number"
      }
    ]
  },
  {
    "key": 5,
    "name": "VAR",
    "code": "x",
    "varNum": "5",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": [
      {
        "ident": "x",
        "varNum": 0,
        "solvedTyp": "Number"
      },
      {
        "ident": "y",
        "varNum": 2,
        "solvedTyp": "Number"
      }
    ]
  },
  {
    "key": 7,
    "name": "VAR",
    "code": "y",
    "varNum": "7",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": [
      {
        "ident": "x",
        "varNum": 0,
        "solvedTyp": "Number"
      },
      {
        "ident": "y",
        "varNum": 2,
        "solvedTyp": "Number"
      }
    ]
  }
];
window.linkDataArray = [
  {
    "from": 10,
    "to": 1
  },
  {
    "from": 10,
    "to": 9
  },
  {
    "from": 9,
    "to": 3
  },
  {
    "from": 9,
    "to": 8
  },
  {
    "from": 8,
    "to": 6
  },
  {
    "from": 8,
    "to": 7
  },
  {
    "from": 6,
    "to": 4
  },
  {
    "from": 6,
    "to": 5
  }
];
        