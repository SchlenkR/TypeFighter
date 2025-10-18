
window.nodeDataArray = [
  {
    "key": 7,
    "name": "LET",
    "code": "myRecord = ...",
    "varNum": "7",
    "additionalInfo": "IDENT = myRecord   TVAR = tv_0   TYP = { age: Number; name: String }",
    "exprTyp": "{ age: Number; name: String }",
    "env": ""
  },
  {
    "key": 5,
    "name": "MK-RECORD",
    "code": "",
    "varNum": "5",
    "additionalInfo": "fields = { age: tv_1; name: tv_3 }",
    "exprTyp": "{ age: Number; name: String }",
    "env": ""
  },
  {
    "key": 1,
    "name": "LIT",
    "code": "22",
    "varNum": "1",
    "additionalInfo": "",
    "exprTyp": "Number",
    "env": ""
  },
  {
    "key": 3,
    "name": "LIT",
    "code": "\u0027John\u0027",
    "varNum": "3",
    "additionalInfo": "",
    "exprTyp": "String",
    "env": ""
  },
  {
    "key": 6,
    "name": "VAR",
    "code": "myRecord",
    "varNum": "6",
    "additionalInfo": "",
    "exprTyp": "{ age: Number; name: String }",
    "env": "myRecord: tv_0 ({ age: Number; name: String })"
  }
];
window.linkDataArray = [
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
    "to": 1
  },
  {
    "from": 5,
    "to": 3
  }
];
        