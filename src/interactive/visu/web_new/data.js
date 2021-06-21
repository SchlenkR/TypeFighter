
window.layout = "tree";

window.nodeDataArray = [
    {
        "key": 0,
        "name": "Let id = ...",
    },
    {
        "key": 1,
        "name": "Fun (x) ->",
    },
    {
        "key": 2,
        "name": "Var (x)",
    },
    {
        "key": 3,
        "name": "App",
    },
    {
        "key": 4,
        "name": "Var (id)",
    },
    {
        "key": 5,
        "name": "Lit (1)",
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
        "to": 5
    }
    , { "from": 3, "to": 1, "category": "constr" }
    , { "from": 1, "to": 5, "category": "constr" }
    , { "from": 1, "to": 5, "category": "constr" }
    , { "from": 1, "to": 5, "category": "constr" }
    , { "from": 2, "to": 5, "category": "constr" }
];
