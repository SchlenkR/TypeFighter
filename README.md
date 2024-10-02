# Type Fighter

Type Fighter is an experimental type solver for ML-style programming languages. It aims to assign types to every term in a program, halting immediately upon encountering errors. The project provides foundational language elements such as functions, let- and do-bindings, and supports type features like records and polymorphic types.

## Table of Contents

- [Examples](#examples)
- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Running Tests in F# Interactive](#running-tests-in-f-interactive)
- [Visualization and Debugging](#visualization-and-debugging)
- [Contributing](#contributing)
- [License](#license)


## Examples 1: A simple program

Given a simple program in pseudo-code:

```fsharp
add 100 10
```

...with the following context provided:

* "add" is a function that takes two numbers and returns a number.

```fsharp
let env =
    [
        "add", Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
    ]
```

The pseudo-code can be translated into an Abstract Syntax Tree (AST) as follows:

```fsharp
(*
    add 100 10
*)

let x = ExprCtx()
let ast = x.App (x.App (x.Var "add") (x.Lit "100")) (x.Lit "10")
```

The AST can be solved using the Type Fighter solver, and the result can be asserted as follows:

```fsharp
ast
|> solve env
|> shouldSolveType (Mono BuiltinTypes.number)
```

The visualized AST of the program looks like this:

![AST for example 1](ast_ex_1.png)

The solver runs recursively, generating a constraint set from the program's AST and the provided context. The constraint set is then solved using a unification algorithm, which assigns types to each term in the program. If a type mismatch is found, the solver halts and reports an error.

Here's how it works internally:

```text
SOLVER RUNS
===========




Solver run 0

  Constraints:
    tv_4 = Number                             :::  4          
    tv_2 = Number                             :::  2          
    tv_1 = (Number -> (Number -> Number))     :::  1          
    tv_1 = (tv_2 -> tv_3)                     :::  3          
    tv_3 = (tv_4 -> tv_5)                     :::  5          

  Solutions:
    <empty>

  Records:





Solver run 1

  Constraints:
    tv_2 = Number                             :::  2          
    tv_1 = (Number -> (Number -> Number))     :::  1          
    tv_1 = (tv_2 -> tv_3)                     :::  3          
    tv_3 = (Number -> tv_5)                   :::  5          

  Solutions:
    tv_4 = Number                          

  Records:





Solver run 2

  Constraints:
    tv_1 = (Number -> (Number -> Number))     :::  1          
    tv_1 = (Number -> tv_3)                   :::  3          
    tv_3 = (Number -> tv_5)                   :::  5          

  Solutions:
    tv_2 = Number                          
    tv_4 = Number                          

  Records:





Solver run 3

  Constraints:
    (Number -> (Number -> Number)) = (Number -> tv_3)                   :::  3          
    tv_3                           = (Number -> tv_5)                   :::  5          

  Solutions:
    tv_1                           = (Number -> (Number -> Number))  
    tv_2                           = Number                          
    tv_4                           = Number                          

  Records:





Solver run 4

  Constraints:
    tv_3 = (Number -> Number)                 :::  3          
    tv_3 = (Number -> tv_5)                   :::  5          

  Solutions:
    tv_1 = (Number -> (Number -> Number))  
    tv_2 = Number                          
    tv_4 = Number                          

  Records:





Solver run 5

  Constraints:
    (Number -> Number) = (Number -> tv_5)                   :::  5          

  Solutions:
    tv_3               = (Number -> Number)              
    tv_1               = (Number -> (Number -> Number))  
    tv_2               = Number                          
    tv_4               = Number                          

  Records:





Solver run 6

  Constraints:
    tv_5 = Number                             :::  5          

  Solutions:
    tv_3 = (Number -> Number)              
    tv_1 = (Number -> (Number -> Number))  
    tv_2 = Number                          
    tv_4 = Number                          

  Records:





Solver run 7

  Constraints:
    <empty>

  Solutions:
    tv_5 = Number                          
    tv_3 = (Number -> Number)              
    tv_1 = (Number -> (Number -> Number))  
    tv_2 = Number                          
    tv_4 = Number                          

  Records:


Final type:
    Number
```


## Examples 2: A more complex program

The program in pseudo-code:

```fsharp
let inst = { IntField = 3, BooleanField = true }
let myFunc = 
    fun r ->
        AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
myFunc inst
```

...with the following context provided:

* "AND" is a function that takes two boolean arguments and returns a boolean.
* "EQUALS" is a polymorphic function that takes two arguments of the same type and returns a boolean.

```fsharp
let env =
    [
        "AND", Mono(BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
        "EQUALS", TDef.Generalize (%1 ^-> %1 ^-> BuiltinTypes.boolean)
    ]
```

The pseudo-code can be translated into an Abstract Syntax Tree (AST) as follows:

```fsharp
(*
    let inst = { IntField = 3, BooleanField = true }
    let myFunc = 
        fun r ->
            AND (EQUALS r.IntField 3) (EQUALS r.BooleanField true)
    myFunc inst
*)
let x = ExprCtx()
let ast =
    x.Let
        (x.Ident "inst")
        (x.MkRecord [
            x.Field "IntField" (x.Lit "3")
            x.Field "BooleanField" (x.Lit "true")
        ])
        (x.Let
            (x.Ident "myFunc")
            (x.Fun (x.Ident "r")
                (x.App
                    (x.App
                        (x.Var "AND")
                        (x.App
                            (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "IntField"))
                            (x.Lit "3")))
                        (x.App
                            (x.App (x.Var "EQUALS") (x.PropAcc (x.Var "r") "BooleanField"))
                            (x.Lit "true"))))
            (x.App
                (x.Var "myFunc")
                (x.Var "inst")))
```

The AST can be solved using the Type Fighter solver, and the result can be asserted as follows:

```fsharp
ast
|> solve defaultTcEnv
|> shouldSolveType (Mono BuiltinTypes.boolean)
```

Run it on your own if you want to see the visualized AST and the type assignments!


## Features

- **Type Inference**: Automatically infers types for expressions in ML-style programs.
- **Error Detection**: Stops execution and reports errors when type mismatches are found.
- **Language and Types**: Supports functions, let-bindings, do-bindings, records, and polymorphic types.
- **AST-Based Programming**: Build programs using an Abstract Syntax Tree (AST) with helper functions.

## Approaches

Type Fighter implements two different approaches to type solving:

1. **Graph-Based Solver**: *Currently not working*. This approach uses a graph representation to solve type equations.
2. **Non-Graph-Based Solver**: *Functional*. This approach operates similarly to solving a system of linear equations.

**Note**: There is no textual programming language or syntax available yet. There's also no runtime or editor provided at the moment.

## Getting Started

### Prerequisites

- [F#](https://fsharp.org/)
- [Node.js](https://nodejs.org/) (for visualization)

### Installation

Clone the repository:

```bash
git clone https://github.com/schlenkr/typefighter.git
cd typefighter
```

Navigate to the non-graph-based solver:

```bash
cd b_nonGraphBasedSolver
```

### Running Tests in F# Interactive

You can explore Type Fighter by running the test cases in F# Interactive:

1. Open TypeFighter.sln in your preferred F# development environment.
2. Review the test cases in src/TypeFighter.Tests/TestCases.
3. Run the tests in F# Interactive to see the AST and type assignments.

## Visualization and Debugging

Type Fighter includes a visualization tool to help you understand the AST and type assignments.

1. Ensure you are in the b_nonGraphBasedSolver directory.
2. Install the required Node.js dependencies:

```bash
npm install
```

3. Start the development server:

```bash
npm run dev
```

4. Open http://localhost:3000 in your browser to view the visualization.

## Contributing

Contributions are welcome! If you have ideas, find bugs, or want to contribute in other ways, please open an issue or submit a pull request.

## License

This project is not available for use in any projects, whether commercial or non-commercial. If you are interested in using this project, please contact me directly for permission.
