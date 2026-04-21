// Curated sample programs grouped by feature. Each one is short enough
// to read without scrolling and uses only identifiers from the Api.fs
// prelude (log / concat / ToString / UnitValue). The "Type inference"
// and "Type errors" sections exist so users can see the type system in
// action — accept programs, reject programs, and explain why.
export type Example = {
    title: string;
    source: string;
};

export type Category = {
    name: string;
    examples: Example[];
};

export const categories: Category[] = [
    {
        name: "Basics",
        examples: [
            {
                title: "hello world",
                source: `let greeting = concat("Hello, ")("World");
log(greeting)`
            },
            {
                title: "literals",
                source: `log(ToString(42));
log(ToString("hello"));
log(ToString(true))`
            },
            {
                title: "floats",
                source: `let pi = 3.14159;
log(ToString(pi))`
            },
            {
                title: "let binding",
                source: `let name = "Ada";
let greeting = concat("Hi, ")(name);
log(greeting)`
            }
        ]
    },
    {
        name: "Functions",
        examples: [
            {
                title: "identity",
                source: `let id = x => x;
log(ToString(id(42)))`
            },
            {
                title: "currying",
                source: `let add = a => b => concat(a)(b);
log(add("foo")("bar"))`
            },
            {
                title: "higher-order",
                source: `let twice = f => x => f(f(x));
let exclaim = s => concat(s)("!");
log(twice(exclaim)("yo"))`
            },
            {
                title: "compose",
                source: `let compose = f => g => x => f(g(x));
let shout = s => concat(s)("!!!");
let greet = s => concat("Hello, ")(s);
log(compose(shout)(greet)("Ada"))`
            }
        ]
    },
    {
        name: "Records",
        examples: [
            {
                title: "property access",
                source: `let p = { age: 22, name: "John" };
log(p.name)`
            },
            {
                title: "nested records",
                source: `let u = { user: { id: 1, name: "Ada" }, active: true };
log(u.user.name)`
            },
            {
                title: "record from function",
                source: `let mkUser = n => a => { name: n, age: a };
let u = mkUser("Ada")(42);
log(u.name)`
            },
            {
                title: "structural accessor",
                source: `let getName = r => r.name;
log(getName({ name: "Ada", age: 42 }));
log(getName({ name: "Bob", age: 31 }))`
            }
        ]
    },
    {
        name: "Arrays",
        examples: [
            {
                title: "array literal",
                source: `let xs = [1, 2, 3];
log(ToString(xs))`
            },
            {
                title: "array of strings",
                source: `let names = ["Ada", "Bob", "Eve"];
log(ToString(names))`
            },
            {
                title: "array of records",
                source: `let points = [
  { x: 1, y: 2 },
  { x: 3, y: 4 }
];
log(ToString(points))`
            }
        ]
    },
    {
        name: "Type inference",
        examples: [
            {
                title: "ToString is polymorphic",
                source: `log(ToString(42));
log(ToString(true));
log(ToString({ x: 1, y: 2 }))`
            },
            {
                title: "inferred function type",
                source: `let greet = s => concat("Hello, ")(s);
log(greet("Ada"))`
            },
            {
                title: "inferred record type",
                source: `let distance = p => p.x;
log(ToString(distance({ x: 10, y: 20 })))`
            },
            {
                title: "function as value",
                source: `let apply = f => x => f(x);
let greet = s => concat("Hi, ")(s);
log(apply(greet)("Ada"))`
            }
        ]
    },
    {
        name: "Type errors",
        examples: [
            {
                title: "concat expects strings",
                source: `concat(42)("hello")`
            },
            {
                title: "heterogeneous array",
                source: `let bad = [1, "two", 3];
log(ToString(bad))`
            },
            {
                title: "number is not a function",
                source: `let x = 42;
x(1)`
            },
            {
                title: "missing record field",
                source: `let p = { x: 1, y: 2 };
log(ToString(p.z))`
            },
            {
                title: "record shape mismatch",
                source: `let useName = r => r.name;
useName({ age: 30 })`
            },
            {
                title: "parse error",
                source: `let x =`
            }
        ]
    }
];
