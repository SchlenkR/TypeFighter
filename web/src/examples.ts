// Curated sample programs grouped by feature. Every example is kept
// short enough to read without scrolling and only uses identifiers from
// the Api.fs prelude (log / concat / ToString / UnitValue). If the
// prelude grows, extend these — or pull from the Parser01/Demos.fs
// fixtures once they're exposed via a shared source.
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
                title: "structural typing",
                source: `let getName = r => r.name;
let user = { name: "Ada", age: 42 };
log(getName(user))`
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
        name: "Type errors",
        examples: [
            {
                title: "wrong argument type",
                source: `log(42)`
            },
            {
                title: "mismatched record fields",
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
