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
                source: `// Simplest program: build a string, hand it to log.
// Both \`concat\` and \`log\` live in the prelude (see "In scope" below).

let greeting = concat("Hello, ")("World");
log(greeting);`
            },
            {
                title: "literals",
                source: `// ToString is polymorphic — the same function accepts any value.
// Hover a name to see its type; ToString has type \`tv_0 -> String\`.

log(ToString(42));
log(ToString("hello"));
log(ToString(true));`
            },
            {
                title: "floats",
                source: `// Number is a single type — no int/float split.

let pi = 3.14159;
log(ToString(pi));`
            },
            {
                title: "let binding",
                source: `// \`let\` introduces a new name for the rest of the program.
// Semicolons separate statements; a trailing one is optional.

let name = "Ada";
let greeting = concat("Hi, ")(name);
log(greeting);`
            }
        ]
    },
    {
        name: "Functions",
        examples: [
            {
                title: "identity",
                source: `// The identity function is fully polymorphic: \`tv_0 -> tv_0\`.
// Hover \`id\` in the source to see the inferred type.

let id = x => x;
log(ToString(id(42)));`
            },
            {
                title: "currying",
                source: `// All multi-arg functions are curried — \`a => b => ...\` is the
// idiomatic shape. Application is one-at-a-time: \`add("foo")("bar")\`.

let add = a => b => concat(a)(b);
log(add("foo")("bar"));`
            },
            {
                title: "higher-order",
                source: `// \`twice\` takes a function and feeds its output back into it.
// Inferred type: \`(tv_0 -> tv_0) -> tv_0 -> tv_0\`.

let twice = f => x => f(f(x));
let exclaim = s => concat(s)("!");
log(twice(exclaim)("yo"));`
            },
            {
                title: "compose",
                source: `// Classic function composition. Three type variables flow through:
// \`compose : (b -> c) -> (a -> b) -> a -> c\`.

let compose = f => g => x => f(g(x));
let shout = s => concat(s)("!!!");
let greet = s => concat("Hello, ")(s);
log(compose(shout)(greet)("Ada"));`
            }
        ]
    },
    {
        name: "Records",
        examples: [
            {
                title: "property access",
                source: `// Records are structural: the type is the set of fields, not a
// declared name. The inferred type here is \`{ age: Number, name: String }\`.

let p = { age: 22, name: "John" };
log(p.name);`
            },
            {
                title: "nested records",
                source: `// Nesting is just a record field whose type is another record.
// Property access chains: \`u.user.name\`.

let u = { user: { id: 1, name: "Ada" }, active: true };
log(u.user.name);`
            },
            {
                title: "record from function",
                source: `// Field order in a literal doesn't matter for the type — records
// are bags of fields, not sequences. This is the pay-off of structural
// typing: \`mkUser\` works the same regardless of which order you write
// \`name\` and \`age\` in its result.

let mkUser = n => a => { name: n, age: a };
let u = mkUser("Ada")(42);
log(u.name);`
            },
            {
                title: "structural accessor",
                source: `// \`getName\` is inferred from its body alone — no type annotations.
// Both calls use the same record shape \`{ name: String, age: Number }\`.

let getName = r => r.name;
log(getName({ name: "Ada", age: 42 }));
log(getName({ name: "Bob", age: 31 }));`
            }
        ]
    },
    {
        name: "Arrays",
        examples: [
            {
                title: "array literal",
                source: `// Arrays are homogeneous — every element must share a type.
// Inferred: \`Array<Number>\`.

let xs = [1, 2, 3];
log(ToString(xs));`
            },
            {
                title: "array of strings",
                source: `// Same shape, different element type — \`Array<String>\`.

let names = ["Ada", "Bob", "Eve"];
log(ToString(names));`
            },
            {
                title: "array of records",
                source: `// Records inside arrays share the element type, which is itself
// a record shape. Inferred: \`Array<{ x: Number, y: Number }>\`.

let points = [
  { x: 1, y: 2 },
  { x: 3, y: 4 }
];
log(ToString(points));`
            }
        ]
    },
    {
        name: "Type inference",
        examples: [
            {
                title: "ToString is polymorphic",
                source: `// ToString's type is \`tv_0 -> String\` — one instantiation per
// call site. Three unrelated argument types, one function.

log(ToString(42));
log(ToString(true));
log(ToString({ x: 1, y: 2 }));`
            },
            {
                title: "inferred function type",
                source: `// No annotations anywhere. The type checker works out:
//   greet : String -> String
// from the shape of concat and "Hello, ".

let greet = s => concat("Hello, ")(s);
log(greet("Ada"));`
            },
            {
                title: "inferred record shape",
                source: `// \`getX\`'s inferred type is \`{ x: tv_0, y: Number } -> tv_0\` —
// the checker figures out both the field set *and* each field's type
// from the call sites, without any annotation on \`getX\` itself.

let getX = p => p.x;
log(ToString(getX({ x: 10, y: 20 })));
log(ToString(getX({ x: 99, y: 100 })));`
            },
            {
                title: "function as value",
                source: `// Functions are first-class values. \`apply\` has the most general
// type a one-arg higher-order function can have:
//   (tv_0 -> tv_1) -> tv_0 -> tv_1

let apply = f => x => f(x);
let greet = s => concat("Hi, ")(s);
log(apply(greet)("Ada"));`
            }
        ]
    },
    {
        name: "Type errors",
        examples: [
            {
                title: "concat expects strings",
                source: `// concat is monomorphic: String -> String -> String.
// Passing a number violates that constraint — the error points
// at the exact mismatch.

concat(42)("hello");`
            },
            {
                title: "heterogeneous array",
                source: `// All array elements must share a type. Mixing Number and String
// has no common type in the prelude, so inference fails.

let bad = [1, "two", 3];
log(ToString(bad));`
            },
            {
                title: "number is not a function",
                source: `// You can only apply values whose inferred type is a function.
// x is a Number, so \`x(1)\` is rejected.

let x = 42;
x(1);`
            },
            {
                title: "missing record field",
                source: `// p's inferred type is \`{ x: Number, y: Number }\`. Accessing .z
// adds a constraint the record doesn't satisfy → type error.

let p = { x: 1, y: 2 };
log(ToString(p.z));`
            },
            {
                title: "record shape mismatch",
                source: `// useName demands \`{ name: tv_0, ... }\`. The argument has \`age\`
// but no \`name\` — the "required field missing" error fires.

let useName = r => r.name;
useName({ age: 30 });`
            },
            {
                title: "parse error",
                source: `// The \`=\` expects an expression on its right — nothing there.
// Parse errors show before type-checking even starts.

let x =`
            }
        ]
    }
];
