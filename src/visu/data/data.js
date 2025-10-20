
window.treesForSolverRuns = [
  {
    "key": 3,
    "name": "MK-RECORD",
    "varNum": 3,
    "code": "",
    "additionalInfo": "fields = { age: tv_0; name: tv_1; address: tv_2 }",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 0,
        "name": "LIT",
        "varNum": 0,
        "code": "22",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [],
        "children": []
      },
      {
        "key": 1,
        "name": "LIT",
        "varNum": 1,
        "code": "\u0027John\u0027",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [],
        "children": []
      },
      {
        "key": 2,
        "name": "LIT",
        "varNum": 2,
        "code": "\u0027123 Main St\u0027",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [],
        "children": []
      }
    ]
  },
  {
    "key": 3,
    "name": "MK-RECORD",
    "varNum": 3,
    "code": "",
    "additionalInfo": "fields = { age: tv_0; name: tv_1; address: tv_2 }",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 0,
        "name": "LIT",
        "varNum": 0,
        "code": "22",
        "additionalInfo": "",
        "exprTyp": "Number",
        "env": [],
        "children": []
      },
      {
        "key": 1,
        "name": "LIT",
        "varNum": 1,
        "code": "\u0027John\u0027",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [],
        "children": []
      },
      {
        "key": 2,
        "name": "LIT",
        "varNum": 2,
        "code": "\u0027123 Main St\u0027",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [],
        "children": []
      }
    ]
  },
  {
    "key": 3,
    "name": "MK-RECORD",
    "varNum": 3,
    "code": "",
    "additionalInfo": "fields = { age: tv_0; name: tv_1; address: tv_2 }",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 0,
        "name": "LIT",
        "varNum": 0,
        "code": "22",
        "additionalInfo": "",
        "exprTyp": "Number",
        "env": [],
        "children": []
      },
      {
        "key": 1,
        "name": "LIT",
        "varNum": 1,
        "code": "\u0027John\u0027",
        "additionalInfo": "",
        "exprTyp": "String",
        "env": [],
        "children": []
      },
      {
        "key": 2,
        "name": "LIT",
        "varNum": 2,
        "code": "\u0027123 Main St\u0027",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [],
        "children": []
      }
    ]
  },
  {
    "key": 3,
    "name": "MK-RECORD",
    "varNum": 3,
    "code": "",
    "additionalInfo": "fields = { age: tv_0; name: tv_1; address: tv_2 }",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 0,
        "name": "LIT",
        "varNum": 0,
        "code": "22",
        "additionalInfo": "",
        "exprTyp": "Number",
        "env": [],
        "children": []
      },
      {
        "key": 1,
        "name": "LIT",
        "varNum": 1,
        "code": "\u0027John\u0027",
        "additionalInfo": "",
        "exprTyp": "String",
        "env": [],
        "children": []
      },
      {
        "key": 2,
        "name": "LIT",
        "varNum": 2,
        "code": "\u0027123 Main St\u0027",
        "additionalInfo": "",
        "exprTyp": "String",
        "env": [],
        "children": []
      }
    ]
  },
  {
    "key": 3,
    "name": "MK-RECORD",
    "varNum": 3,
    "code": "",
    "additionalInfo": "fields = { age: tv_0; name: tv_1; address: tv_2 }",
    "exprTyp": "{ address: String; age: Number; name: String }",
    "env": [],
    "children": [
      {
        "key": 0,
        "name": "LIT",
        "varNum": 0,
        "code": "22",
        "additionalInfo": "",
        "exprTyp": "Number",
        "env": [],
        "children": []
      },
      {
        "key": 1,
        "name": "LIT",
        "varNum": 1,
        "code": "\u0027John\u0027",
        "additionalInfo": "",
        "exprTyp": "String",
        "env": [],
        "children": []
      },
      {
        "key": 2,
        "name": "LIT",
        "varNum": 2,
        "code": "\u0027123 Main St\u0027",
        "additionalInfo": "",
        "exprTyp": "String",
        "env": [],
        "children": []
      }
    ]
  }
];
window.solverRuns = [
  {
    "constraints": [
      {
        "t1": "tv_0",
        "t2": "Number"
      },
      {
        "t1": "tv_1",
        "t2": "String"
      },
      {
        "t1": "tv_2",
        "t2": "String"
      },
      {
        "t1": "tv_3",
        "t2": "{ address: tv_2; age: tv_0; name: tv_1 }"
      }
    ],
    "solutions": [],
    "recordRefs": [],
    "error": null
  },
  {
    "constraints": [
      {
        "t1": "tv_1",
        "t2": "String"
      },
      {
        "t1": "tv_2",
        "t2": "String"
      },
      {
        "t1": "tv_3",
        "t2": "{ address: tv_2; age: Number; name: tv_1 }"
      }
    ],
    "solutions": [
      {
        "t1": "tv_0",
        "t2": "Number"
      }
    ],
    "recordRefs": [],
    "error": null
  },
  {
    "constraints": [
      {
        "t1": "tv_2",
        "t2": "String"
      },
      {
        "t1": "tv_3",
        "t2": "{ address: tv_2; age: Number; name: String }"
      }
    ],
    "solutions": [
      {
        "t1": "tv_1",
        "t2": "String"
      },
      {
        "t1": "tv_0",
        "t2": "Number"
      }
    ],
    "recordRefs": [],
    "error": null
  },
  {
    "constraints": [
      {
        "t1": "tv_3",
        "t2": "{ address: String; age: Number; name: String }"
      }
    ],
    "solutions": [
      {
        "t1": "tv_2",
        "t2": "String"
      },
      {
        "t1": "tv_1",
        "t2": "String"
      },
      {
        "t1": "tv_0",
        "t2": "Number"
      }
    ],
    "recordRefs": [],
    "error": null
  },
  {
    "constraints": [],
    "solutions": [
      {
        "t1": "tv_3",
        "t2": "{ address: String; age: Number; name: String }"
      },
      {
        "t1": "tv_2",
        "t2": "String"
      },
      {
        "t1": "tv_1",
        "t2": "String"
      },
      {
        "t1": "tv_0",
        "t2": "Number"
      }
    ],
    "recordRefs": [],
    "error": null
  }
];
        