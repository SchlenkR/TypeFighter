// Single source of truth for the TypeFighter-Web language built-ins. Each
// entry carries both its type signature (drives the solver prelude in `Api`)
// and its curried `Func<object,object>` C# closure (drives the runtime imports
// in `Eval`, used when a built-in appears as a first-class value). Adding a
// built-in is one edit here instead of keeping a prelude list and an import
// list in two files in sync.
//
// NOTE: `useMemo` / `count` are NOT here -- they live in the stdlib (see
// `Api.stdlibSource`) as ordinary library functions producing a step-record.
module PureState.TypeFighter.Web.Builtins

open PureState.TypeFighter

type Builtin =
    {
        name: string
        typ: Typ
        csClosure: string
    }

let all : Builtin list =
    [
        {
            name = "log"
            typ = TDef.Generalize (%0 ^-> BuiltinTypes.unit)
            csClosure =
                "(Func<object, object>)((object s) => { __out.WriteLine(s == null ? \"\" : s.ToString()); return null; })"
        }
        // Internal: reads the current sample time (seconds since Unix epoch) from
        // the emit's `__now` parameter. The user-facing `now` stdlib stream wraps
        // it. Emit lowers it directly (NowLike); the closure is only a first-class
        // fallback (no `__now` in scope there, so 0).
        {
            name = "__nowSeconds"
            typ = Mono (BuiltinTypes.unit ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object _) => (object)0D)"
        }
        {
            name = "concat"
            typ = Mono (BuiltinTypes.string ^-> BuiltinTypes.string ^-> BuiltinTypes.string)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)((string)a + (string)b)))"
        }
        {
            name = BuiltinValues.unitValueIdent
            typ = Mono BuiltinTypes.unit
            csClosure = "(object)null"
        }
        {
            name = BuiltinValues.toStringFunctionIdent
            typ = TDef.Generalize (%0 ^-> BuiltinTypes.string)
            csClosure =
                "(Func<object, object>)((object x) => (object)(x == null ? \"\" : x.ToString()))"
        }
        // Array access: `head` returns the first element, `length` the count,
        // `at` returns the i-th element (0-based). Surface syntax `xs[i]`
        // desugars to `at(xs)(i)`.
        {
            name = "head"
            typ = TDef.Generalize (BuiltinTypes.array %0 ^-> %0)
            csClosure = "(Func<object, object>)((object xs) => ((object[])xs)[0])"
        }
        {
            name = "length"
            typ = TDef.Generalize (BuiltinTypes.array %0 ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object xs) => (object)(double)((object[])xs).Length)"
        }
        {
            name = "at"
            typ = TDef.Generalize (BuiltinTypes.array %0 ^-> BuiltinTypes.number ^-> %0)
            csClosure =
                "(Func<object, object>)((object xs) => (Func<object, object>)((object i) => ((object[])xs)[(int)(double)i]))"
        }
        // Operators with constrained polymorphism: `+` over Number|String,
        // arithmetic over Number, comparisons over Number|String returning Boolean.
        {
            name = "op_add"
            typ =
                TDef.GeneralizeWithConstraints
                    [ 0, [ BuiltinTypes.number; BuiltinTypes.string ] ]
                    (%0 ^-> %0 ^-> %0)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (a is string sa) ? (object)(sa + (string)b) : (object)((double)a + (double)b)))"
        }
        {
            name = "op_sub"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)((double)a - (double)b)))"
        }
        {
            name = "op_mul"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)((double)a * (double)b)))"
        }
        {
            name = "op_div"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)((double)a / (double)b)))"
        }
        {
            name = "op_lt"
            typ =
                TDef.GeneralizeWithConstraints
                    [ 0, [ BuiltinTypes.number; BuiltinTypes.string ] ]
                    (%0 ^-> %0 ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (a is string sa) ? (object)(string.Compare(sa, (string)b) < 0) : (object)((double)a < (double)b)))"
        }
        {
            name = "op_gt"
            typ =
                TDef.GeneralizeWithConstraints
                    [ 0, [ BuiltinTypes.number; BuiltinTypes.string ] ]
                    (%0 ^-> %0 ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (a is string sa) ? (object)(string.Compare(sa, (string)b) > 0) : (object)((double)a > (double)b)))"
        }
        {
            name = "op_le"
            typ =
                TDef.GeneralizeWithConstraints
                    [ 0, [ BuiltinTypes.number; BuiltinTypes.string ] ]
                    (%0 ^-> %0 ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (a is string sa) ? (object)(string.Compare(sa, (string)b) <= 0) : (object)((double)a <= (double)b)))"
        }
        {
            name = "op_ge"
            typ =
                TDef.GeneralizeWithConstraints
                    [ 0, [ BuiltinTypes.number; BuiltinTypes.string ] ]
                    (%0 ^-> %0 ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (a is string sa) ? (object)(string.Compare(sa, (string)b) >= 0) : (object)((double)a >= (double)b)))"
        }
        {
            name = "op_eq"
            typ = TDef.Generalize (%0 ^-> %0 ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)object.Equals(a, b)))"
        }
        {
            name = "op_neq"
            typ = TDef.Generalize (%0 ^-> %0 ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)(!object.Equals(a, b))))"
        }
        {
            name = "op_and"
            typ = Mono (BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)((bool)a && (bool)b)))"
        }
        {
            name = "op_or"
            typ = Mono (BuiltinTypes.boolean ^-> BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)((bool)a || (bool)b)))"
        }
        {
            name = "op_not"
            typ = Mono (BuiltinTypes.boolean ^-> BuiltinTypes.boolean)
            csClosure = "(Func<object, object>)((object a) => (object)(!(bool)a))"
        }
        {
            name = "op_neg"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)(-(double)a))"
        }
        // Branching as a builtin, not a syntactic form. Eager in both branches
        // (function-call semantics), but the pure surface language makes that
        // safe -- there are no side effects to skip. Emit lowers fully-applied
        // calls to a typed C# ternary.  cond : <T>. Bool -> T -> T -> T
        {
            name = "cond"
            typ = TDef.Generalize (BuiltinTypes.boolean ^-> %0 ^-> %0 ^-> %0)
            csClosure =
                "(Func<object, object>)((object b) => (Func<object, object>)((object t) => (Func<object, object>)((object e) => ((bool)b ? t : e))))"
        }
        // Math over Number. Backbone of the stdlib stream blocks (sine,
        // rescale, rollingStdDev, ...).
        {
            name = "sin"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)Math.Sin((double)a))"
        }
        {
            name = "cos"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)Math.Cos((double)a))"
        }
        {
            name = "sqrt"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)Math.Sqrt((double)a))"
        }
        {
            name = "abs"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)Math.Abs((double)a))"
        }
        {
            name = "floor"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)Math.Floor((double)a))"
        }
        {
            name = "ceil"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)Math.Ceiling((double)a))"
        }
        {
            name = "round"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure = "(Func<object, object>)((object a) => (object)Math.Round((double)a))"
        }
        {
            name = "pow"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)Math.Pow((double)a, (double)b)))"
        }
        {
            name = "min"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)Math.Min((double)a, (double)b)))"
        }
        {
            name = "max"
            typ = Mono (BuiltinTypes.number ^-> BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (object)Math.Max((double)a, (double)b)))"
        }
        // Array helpers for the sliding-window stream blocks. All TOTAL:
        // `skipFirst([])` is `[]`, `sumOf([])` is 0, the other aggregates
        // yield NaN on `[]` -- in a VirtualCurves sampler NaN is a gap,
        // which is exactly right for a window that hasn't filled yet.
        {
            name = "append"
            typ = TDef.Generalize (BuiltinTypes.array %0 ^-> %0 ^-> BuiltinTypes.array %0)
            csClosure =
                "(Func<object, object>)((object xs) => (Func<object, object>)((object x) => { var a = (object[])xs; var r = new object[a.Length + 1]; Array.Copy(a, r, a.Length); r[a.Length] = x; return (object)r; }))"
        }
        {
            name = "skipFirst"
            typ = TDef.Generalize (BuiltinTypes.array %0 ^-> BuiltinTypes.array %0)
            csClosure =
                "(Func<object, object>)((object xs) => { var a = (object[])xs; if (a.Length == 0) return (object)a; var r = new object[a.Length - 1]; Array.Copy(a, 1, r, 0, r.Length); return (object)r; })"
        }
        {
            name = "sumOf"
            typ = Mono (BuiltinTypes.array BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object xs) => { var a = (object[])xs; double s = 0; for (int i = 0; i < a.Length; i++) s += (double)a[i]; return (object)s; })"
        }
        {
            name = "avgOf"
            typ = Mono (BuiltinTypes.array BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object xs) => { var a = (object[])xs; if (a.Length == 0) return (object)double.NaN; double s = 0; for (int i = 0; i < a.Length; i++) s += (double)a[i]; return (object)(s / a.Length); })"
        }
        {
            name = "minOf"
            typ = Mono (BuiltinTypes.array BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object xs) => { var a = (object[])xs; if (a.Length == 0) return (object)double.NaN; double m = (double)a[0]; for (int i = 1; i < a.Length; i++) m = Math.Min(m, (double)a[i]); return (object)m; })"
        }
        {
            name = "maxOf"
            typ = Mono (BuiltinTypes.array BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object xs) => { var a = (object[])xs; if (a.Length == 0) return (object)double.NaN; double m = (double)a[0]; for (int i = 1; i < a.Length; i++) m = Math.Max(m, (double)a[i]); return (object)m; })"
        }
        {
            name = "stdDevOf"
            typ = Mono (BuiltinTypes.array BuiltinTypes.number ^-> BuiltinTypes.number)
            csClosure =
                "(Func<object, object>)((object xs) => { var a = (object[])xs; if (a.Length == 0) return (object)double.NaN; double s = 0; for (int i = 0; i < a.Length; i++) s += (double)a[i]; double mean = s / a.Length; double v = 0; for (int i = 0; i < a.Length; i++) { double d = (double)a[i] - mean; v += d * d; } return (object)Math.Sqrt(v / a.Length); })"
        }
    ]

// ─── Imported stream blocks (vendored DSP library) ────────────
// Declared purely by TYPE: a step-record-typed PARAMETER receives
// its argument un-desugared (the desugar pass wraps plain values
// into constant streams and re-wraps AST streams), a step-record
// RETURN makes call sites stream-lets the lift picks up. At
// runtime the closure builds the protocol record via `DspRt`;
// the emit drives any such opaque stream generically.
//
// Blocks live under dotted NAMESPACES: users write `math.ema(...)`,
// `gen.counter(...)`. The desugar pass rewrites `ns.member` to the
// C#-safe flat name `__<ns>_<member>`; one record-typed prelude
// entry per namespace drives typing, hover and dot-completion.

// Signature abbreviations for the block table below.
let private num = BuiltinTypes.number
let private srNum s = BuiltinTypes.stepRecord BuiltinTypes.number s
let private srBool s = BuiltinTypes.stepRecord BuiltinTypes.boolean s

/// (namespace, member, type, runtime closure) -- single source of
/// truth for the imported blocks.
let importedBlocks : (string * string * Typ * string) list =
    [
        // math: statistics, arithmetic, windowing
        "math", "ema",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object alpha) => PureState.TypeFighter.Web.DspRt.Ema(src, (double)alpha)))"
        "math", "movingAverage",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.MovingAverage(src, (double)n)))"
        "math", "rollingStdDev",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.RollingStdDev(src, (double)n)))"
        "math", "rollingMin",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.RollingMin(src, (double)n)))"
        "math", "rollingMax",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.RollingMax(src, (double)n)))"
        "math", "runningMin",
            TDef.Generalize (srNum %0 ^-> srNum %1),
            "(Func<object, object>)((object src) => PureState.TypeFighter.Web.DspRt.RunningMin(src))"
        "math", "runningMax",
            TDef.Generalize (srNum %0 ^-> srNum %1),
            "(Func<object, object>)((object src) => PureState.TypeFighter.Web.DspRt.RunningMax(src))"
        "math", "diff",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object seed) => PureState.TypeFighter.Web.DspRt.Diff(src, (double)seed)))"
        "math", "cumulativeSum",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object seed) => PureState.TypeFighter.Web.DspRt.CumulativeSum(src, (double)seed)))"
        "math", "cumulativeProduct",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object seed) => PureState.TypeFighter.Web.DspRt.CumulativeProduct(src, (double)seed)))"
        "math", "clamp",
            TDef.Generalize (srNum %0 ^-> num ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object lo) => (Func<object, object>)((object hi) => PureState.TypeFighter.Web.DspRt.Clamp(src, (double)lo, (double)hi))))"
        "math", "rescale",
            TDef.Generalize (srNum %0 ^-> num ^-> num ^-> num ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object inLo) => (Func<object, object>)((object inHi) => (Func<object, object>)((object outLo) => (Func<object, object>)((object outHi) => PureState.TypeFighter.Web.DspRt.Rescale(src, (double)inLo, (double)inHi, (double)outLo, (double)outHi))))))"
        "math", "windowedReduce",
            TDef.Generalize (srNum %0 ^-> num ^-> (num ^-> num ^-> num) ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => (Func<object, object>)((object f) => (Func<object, object>)((object seed) => PureState.TypeFighter.Web.DspRt.WindowedReduce(src, (double)n, (Func<object, object>)f, (double)seed)))))"
        // gen: generators and oscillators
        "gen", "counter",
            TDef.Generalize (num ^-> num ^-> srNum %0),
            "(Func<object, object>)((object start) => (Func<object, object>)((object inc) => PureState.TypeFighter.Web.DspRt.Counter((double)start, (double)inc)))"
        // Plain stream VALUE (like `count`), not a function.
        "gen", "fibonacci",
            TDef.Generalize (srNum %0),
            "PureState.TypeFighter.Web.DspRt.Fibonacci()"
        "gen", "whiteNoise",
            TDef.Generalize (num ^-> srNum %0),
            "(Func<object, object>)((object seed) => PureState.TypeFighter.Web.DspRt.WhiteNoise((double)seed))"
        "gen", "sineOsc",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object freq) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.SineOsc(freq, (double)sr)))"
        "gen", "sawOsc",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object freq) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.SawOsc(freq, (double)sr)))"
        "gen", "squareOsc",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> num ^-> srNum %2),
            "(Func<object, object>)((object freq) => (Func<object, object>)((object pw) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.SquareOsc(freq, pw, (double)sr))))"
        "gen", "triangleOsc",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object freq) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.TriangleOsc(freq, (double)sr)))"
        // logic: state, gating, counting, detection, delay
        "logic", "hold",
            TDef.Generalize (srNum %0 ^-> (num ^-> BuiltinTypes.boolean) ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object pred) => (Func<object, object>)((object def) => PureState.TypeFighter.Web.DspRt.Hold(src, (Func<object, object>)pred, (double)def))))"
        "logic", "latch",
            TDef.Generalize (srBool %0 ^-> srNum %1 ^-> num ^-> srNum %2),
            "(Func<object, object>)((object trigger) => (Func<object, object>)((object src) => (Func<object, object>)((object def) => PureState.TypeFighter.Web.DspRt.Latch(trigger, src, (double)def))))"
        // Rising edge = 1, falling edge = -1, otherwise 0.
        "logic", "edge",
            TDef.Generalize (srBool %0 ^-> srNum %1),
            "(Func<object, object>)((object trigger) => PureState.TypeFighter.Web.DspRt.Edge(trigger))"
        "logic", "changed",
            TDef.Generalize (srNum %0 ^-> srBool %1),
            "(Func<object, object>)((object src) => PureState.TypeFighter.Web.DspRt.Changed(src))"
        "logic", "debounce",
            TDef.Generalize (srBool %0 ^-> num ^-> srBool %1),
            "(Func<object, object>)((object trigger) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.Debounce(trigger, (double)n)))"
        "logic", "toggle",
            TDef.Generalize (srBool %0 ^-> srBool %1),
            "(Func<object, object>)((object trigger) => PureState.TypeFighter.Web.DspRt.Toggle(trigger))"
        "logic", "countWhere",
            TDef.Generalize (srNum %0 ^-> (num ^-> BuiltinTypes.boolean) ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object pred) => PureState.TypeFighter.Web.DspRt.CountWhere(src, (Func<object, object>)pred)))"
        "logic", "countSince",
            TDef.Generalize (srBool %0 ^-> srNum %1),
            "(Func<object, object>)((object trigger) => PureState.TypeFighter.Web.DspRt.CountSince(trigger))"
        "logic", "countIn",
            TDef.Generalize (srBool %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object trigger) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.CountIn(trigger, (double)n)))"
        "logic", "rate",
            TDef.Generalize (srBool %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object trigger) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.Rate(trigger, (double)n)))"
        "logic", "threshold",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object level) => PureState.TypeFighter.Web.DspRt.Threshold(src, (double)level)))"
        // a crosses above b = 1, below = -1, otherwise 0.
        "logic", "crossover",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> srNum %2),
            "(Func<object, object>)((object a) => (Func<object, object>)((object b) => PureState.TypeFighter.Web.DspRt.Crossover(a, b)))"
        "logic", "delayBy1",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object def) => PureState.TypeFighter.Web.DspRt.DelayBy1(src, (double)def)))"
        "logic", "delayByN",
            TDef.Generalize (srNum %0 ^-> num ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => (Func<object, object>)((object def) => PureState.TypeFighter.Web.DspRt.DelayByN(src, (double)n, (double)def))))"
        // dsp: filters, envelopes, dynamics, modulation, analysis
        // (cutoff/center/q are modulatable input streams)
        "dsp", "lowPass1",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> num ^-> srNum %2),
            "(Func<object, object>)((object src) => (Func<object, object>)((object cutoff) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.LowPass1(src, cutoff, (double)sr))))"
        "dsp", "highPass1",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> num ^-> srNum %2),
            "(Func<object, object>)((object src) => (Func<object, object>)((object cutoff) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.HighPass1(src, cutoff, (double)sr))))"
        "dsp", "biquadLowPass",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> srNum %2 ^-> num ^-> srNum %3),
            "(Func<object, object>)((object src) => (Func<object, object>)((object cutoff) => (Func<object, object>)((object q) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.BiquadLowPass(src, cutoff, q, (double)sr)))))"
        "dsp", "biquadHighPass",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> srNum %2 ^-> num ^-> srNum %3),
            "(Func<object, object>)((object src) => (Func<object, object>)((object cutoff) => (Func<object, object>)((object q) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.BiquadHighPass(src, cutoff, q, (double)sr)))))"
        "dsp", "biquadBandPass",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> srNum %2 ^-> num ^-> srNum %3),
            "(Func<object, object>)((object src) => (Func<object, object>)((object center) => (Func<object, object>)((object q) => (Func<object, object>)((object sr) => PureState.TypeFighter.Web.DspRt.BiquadBandPass(src, center, q, (double)sr)))))"
        "dsp", "dcBlock",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object r) => PureState.TypeFighter.Web.DspRt.DcBlock(src, (double)r)))"
        "dsp", "envFollow",
            TDef.Generalize (srNum %0 ^-> num ^-> num ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object sr) => (Func<object, object>)((object atk) => (Func<object, object>)((object rel) => PureState.TypeFighter.Web.DspRt.EnvFollow(src, (double)sr, (double)atk, (double)rel)))))"
        "dsp", "adsr",
            TDef.Generalize (srBool %0 ^-> num ^-> num ^-> num ^-> num ^-> srNum %1),
            "(Func<object, object>)((object gate) => (Func<object, object>)((object a) => (Func<object, object>)((object d) => (Func<object, object>)((object s) => (Func<object, object>)((object r) => PureState.TypeFighter.Web.DspRt.Adsr(gate, (double)a, (double)d, (double)s, (double)r))))))"
        "dsp", "softClip",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object drive) => PureState.TypeFighter.Web.DspRt.SoftClip(src, (double)drive)))"
        "dsp", "hardClip",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object t) => PureState.TypeFighter.Web.DspRt.HardClip(src, (double)t)))"
        "dsp", "gate",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> num ^-> srNum %2),
            "(Func<object, object>)((object src) => (Func<object, object>)((object env) => (Func<object, object>)((object t) => PureState.TypeFighter.Web.DspRt.Gate(src, env, (double)t))))"
        "dsp", "ringMod",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> srNum %2),
            "(Func<object, object>)((object a) => (Func<object, object>)((object b) => PureState.TypeFighter.Web.DspRt.RingMod(a, b)))"
        "dsp", "bitCrush",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object bits) => PureState.TypeFighter.Web.DspRt.BitCrush(src, (double)bits)))"
        "dsp", "crossfade",
            TDef.Generalize (srNum %0 ^-> srNum %1 ^-> srNum %2 ^-> srNum %3),
            "(Func<object, object>)((object a) => (Func<object, object>)((object b) => (Func<object, object>)((object mix) => PureState.TypeFighter.Web.DspRt.Crossfade(a, b, mix))))"
        "dsp", "rms",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.Rms(src, (double)n)))"
        "dsp", "zeroCrossRate",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object n) => PureState.TypeFighter.Web.DspRt.ZeroCrossRate(src, (double)n)))"
        "dsp", "peakHold",
            TDef.Generalize (srNum %0 ^-> num ^-> srNum %1),
            "(Func<object, object>)((object src) => (Func<object, object>)((object decay) => PureState.TypeFighter.Web.DspRt.PeakHold(src, (double)decay)))"
    ]

let private flatNameOf ns memberName = "__" + ns + "_" + memberName

let private monoOf typ =
    match typ with
    | Mono m -> m
    | Poly p -> p.monoTyp

let private blocksByNamespace =
    importedBlocks |> List.groupBy (fun (ns, _, _, _) -> ns)

// One record-typed prelude entry per namespace: a record whose fields
// are the member signatures. Drives typing, hover and dot-completion;
// has no runtime value (members resolve to the flat imports).
let private namespacePrelude : (string * Typ) list =
    [ for ns, blocks in blocksByNamespace ->
        ns, Mono (TDef.RecordWith [ for _, m, typ, _ in blocks -> m, monoOf typ ]) ]

/// ns -> member -> flat runtime name; feeds the desugar oracle that
/// rewrites `ns.member` to the flat prelude name.
let namespaceMembers : Map<string, Map<string, string>> =
    Map.ofList [
        for ns, blocks in blocksByNamespace ->
            ns, Map.ofList [ for _, m, _, _ in blocks -> m, flatNameOf ns m ]
    ]

/// Type signatures for the solver prelude.
let prelude : (string * Typ) list =
    [ for x in all -> x.name, x.typ ]
    @ [ for ns, m, typ, _ in importedBlocks -> flatNameOf ns m, typ ]
    @ namespacePrelude

/// Curried `Func<object,object>` closures for the runtime imports.
let csImports : (string * string) list =
    [ for x in all -> x.name, x.csClosure ]
    @ [ for ns, m, _, closure in importedBlocks -> flatNameOf ns m, closure ]
