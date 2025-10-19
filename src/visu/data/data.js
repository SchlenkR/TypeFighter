
window.treesForSolverRuns = [
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = ",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": ""
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": ""
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": ""
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": ""
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": ""
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": ""
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = ",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": ""
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": ""
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": ""
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": ""
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": ""
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "tv_1",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": ""
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(7)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(7)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(7)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(7)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(7)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(7)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(7)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(7)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(7)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(7)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(7)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(7)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(7)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(7)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(7)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(7)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(7)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(7)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(7)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(7)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_4",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_4",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_4",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_8",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = ",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": ""
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": ""
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": ""
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "tv_18",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "tv_18",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "Number",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "tv_18",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "Number",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "Bool",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "tv_18",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "{ BooleanField: Bool; IntField: Number }",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "Number",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "Bool",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "tv_18",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "(recordRef_(3) -\u003E Bool)",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "{ BooleanField: Bool; IntField: Number }",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "Number",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "Bool",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "tv_18",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(tv_20 -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "tv_20",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "tv_20",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "(recordRef_(3) -\u003E Bool)",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "{ BooleanField: Bool; IntField: Number }",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "Number",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "Bool",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "key": 19,
    "name": "LET",
    "varNum": 19,
    "code": "myFunc = ...",
    "additionalInfo": "IDENT = myFunc   TVAR = tv_0   TYP = (recordRef_(3) -\u003E Bool)",
    "exprTyp": "tv_18",
    "env": [],
    "children": [
      {
        "key": 11,
        "name": "FUN",
        "varNum": 11,
        "code": "r -\u003E ...",
        "additionalInfo": "IDENT = r   TVAR = tv_1   TYP = recordRef_(3)",
        "exprTyp": "(recordRef_(3) -\u003E Bool)",
        "env": [],
        "children": [
          {
            "key": 10,
            "name": "APP",
            "varNum": 10,
            "code": "",
            "additionalInfo": "",
            "exprTyp": "Bool",
            "env": [
              {
                "ident": "r",
                "varNum": 1,
                "solvedTyp": "recordRef_(3)"
              }
            ],
            "children": [
              {
                "key": 6,
                "name": "APP",
                "varNum": 6,
                "code": "",
                "additionalInfo": "",
                "exprTyp": "(Bool -\u003E Bool)",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 2,
                    "name": "VAR",
                    "varNum": 2,
                    "code": "EQUALS",
                    "additionalInfo": "",
                    "exprTyp": "(Bool -\u003E (Bool -\u003E Bool))",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  },
                  {
                    "key": 5,
                    "name": "PROP-ACC",
                    "varNum": 5,
                    "code": "_.IntField",
                    "additionalInfo": "var(ident) = tv_4",
                    "exprTyp": "Bool",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": [
                      {
                        "key": 3,
                        "name": "VAR",
                        "varNum": 3,
                        "code": "r",
                        "additionalInfo": "",
                        "exprTyp": "recordRef_(3)",
                        "env": [
                          {
                            "ident": "r",
                            "varNum": 1,
                            "solvedTyp": "recordRef_(3)"
                          }
                        ],
                        "children": []
                      }
                    ]
                  }
                ]
              },
              {
                "key": 9,
                "name": "PROP-ACC",
                "varNum": 9,
                "code": "_.BooleanField",
                "additionalInfo": "var(ident) = tv_8",
                "exprTyp": "Bool",
                "env": [
                  {
                    "ident": "r",
                    "varNum": 1,
                    "solvedTyp": "recordRef_(3)"
                  }
                ],
                "children": [
                  {
                    "key": 7,
                    "name": "VAR",
                    "varNum": 7,
                    "code": "r",
                    "additionalInfo": "",
                    "exprTyp": "recordRef_(3)",
                    "env": [
                      {
                        "ident": "r",
                        "varNum": 1,
                        "solvedTyp": "recordRef_(3)"
                      }
                    ],
                    "children": []
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "key": 18,
        "name": "APP",
        "varNum": 18,
        "code": "",
        "additionalInfo": "",
        "exprTyp": "",
        "env": [
          {
            "ident": "myFunc",
            "varNum": 0,
            "solvedTyp": "(recordRef_(3) -\u003E Bool)"
          }
        ],
        "children": [
          {
            "key": 12,
            "name": "VAR",
            "varNum": 12,
            "code": "myFunc",
            "additionalInfo": "",
            "exprTyp": "(recordRef_(3) -\u003E Bool)",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": []
          },
          {
            "key": 17,
            "name": "MK-RECORD",
            "varNum": 17,
            "code": "",
            "additionalInfo": "fields = { IntField: tv_13; BooleanField: tv_15 }",
            "exprTyp": "{ BooleanField: Bool; IntField: Number }",
            "env": [
              {
                "ident": "myFunc",
                "varNum": 0,
                "solvedTyp": "(recordRef_(3) -\u003E Bool)"
              }
            ],
            "children": [
              {
                "key": 13,
                "name": "LIT",
                "varNum": 13,
                "code": "3",
                "additionalInfo": "",
                "exprTyp": "Number",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              },
              {
                "key": 15,
                "name": "LIT",
                "varNum": 15,
                "code": "True",
                "additionalInfo": "",
                "exprTyp": "Bool",
                "env": [
                  {
                    "ident": "myFunc",
                    "varNum": 0,
                    "solvedTyp": "(recordRef_(3) -\u003E Bool)"
                  }
                ],
                "children": []
              }
            ]
          }
        ]
      }
    ]
  }
];
window.solverRuns = [
  {
    "constraints": [
      {
        "t1": "tv_7",
        "t2": "tv_1"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_3",
        "t2": "tv_1"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_5 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_9 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(tv_1 -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      },
      {
        "key": 7,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_1",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_3",
        "t2": "tv_1"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_5 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_9 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(tv_1 -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_7",
        "t2": "tv_1"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      },
      {
        "key": 7,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_5 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_9 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(7) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_1",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(7)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      },
      {
        "key": 7,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_3",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_5 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(7) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(7)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      },
      {
        "key": 7,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "recordRef_(7)",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_5 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(7) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_3",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(7)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(7)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      },
      {
        "key": 7,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_5 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          },
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_4 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          },
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "(tv_20 -\u003E (tv_20 -\u003E Bool))",
        "t2": "(tv_4 -\u003E tv_6)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          },
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_4"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          },
          {
            "name": "IntField",
            "typ": "tv_4"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "(tv_20 -\u003E Bool)",
        "t2": "(tv_8 -\u003E tv_10)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_8"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_8"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E tv_10)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_0",
        "t2": "tv_11"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "tv_0"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: tv_13 }"
      },
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: tv_15; IntField: Number }"
      },
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: Bool; IntField: Number }"
      },
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_12",
        "t2": "(tv_17 -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_12",
        "t2": "({ BooleanField: Bool; IntField: Number } -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: Bool; IntField: Number }"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "(recordRef_(3) -\u003E Bool)",
        "t2": "({ BooleanField: Bool; IntField: Number } -\u003E tv_18)"
      }
    ],
    "solutions": [
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: Bool; IntField: Number }"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "tv_20",
        "t2": "Bool"
      },
      {
        "t1": "tv_20",
        "t2": "Number"
      },
      {
        "t1": "tv_18",
        "t2": "Bool"
      }
    ],
    "solutions": [
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: Bool; IntField: Number }"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "tv_20"
      },
      {
        "t1": "tv_6",
        "t2": "(tv_20 -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "tv_20"
      },
      {
        "t1": "tv_2",
        "t2": "(tv_20 -\u003E (tv_20 -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "tv_20"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "tv_20"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "tv_20"
          },
          {
            "name": "IntField",
            "typ": "tv_20"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  },
  {
    "constraints": [
      {
        "t1": "Bool",
        "t2": "Number"
      },
      {
        "t1": "tv_18",
        "t2": "Bool"
      }
    ],
    "solutions": [
      {
        "t1": "tv_20",
        "t2": "Bool"
      },
      {
        "t1": "tv_12",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_17",
        "t2": "{ BooleanField: Bool; IntField: Number }"
      },
      {
        "t1": "tv_15",
        "t2": "Bool"
      },
      {
        "t1": "tv_13",
        "t2": "Number"
      },
      {
        "t1": "tv_19",
        "t2": "tv_18"
      },
      {
        "t1": "tv_0",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_11",
        "t2": "(recordRef_(3) -\u003E Bool)"
      },
      {
        "t1": "tv_10",
        "t2": "Bool"
      },
      {
        "t1": "tv_8",
        "t2": "Bool"
      },
      {
        "t1": "tv_6",
        "t2": "(Bool -\u003E Bool)"
      },
      {
        "t1": "tv_4",
        "t2": "Bool"
      },
      {
        "t1": "tv_2",
        "t2": "(Bool -\u003E (Bool -\u003E Bool))"
      },
      {
        "t1": "tv_5",
        "t2": "Bool"
      },
      {
        "t1": "tv_3",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_9",
        "t2": "Bool"
      },
      {
        "t1": "tv_1",
        "t2": "recordRef_(3)"
      },
      {
        "t1": "tv_7",
        "t2": "recordRef_(3)"
      }
    ],
    "recordRefs": [
      {
        "key": 3,
        "fields": [
          {
            "name": "BooleanField",
            "typ": "Bool"
          },
          {
            "name": "IntField",
            "typ": "Bool"
          }
        ]
      }
    ],
    "error": "Can\u0027t unify\n    Bool\n  and\n    Number\nReason: Can\u0027t unify types \u0027Bool\u0027 and \u0027Number\u0027\nSource Expression: App Var myFunc ...\nSource TVar: tv_18\n"
  }
];
        