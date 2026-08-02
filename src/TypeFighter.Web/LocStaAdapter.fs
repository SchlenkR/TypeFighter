// Bridges LocSta2 SigStreams into the TypeFighter runtime step protocol.
// A step-record VALUE is an IDictionary<string,obj> { __init; __step } where
// __step is a Func<obj,obj> from state to { val; nextState }. State flows
// through the protocol (held in the emit's slot), so every factory here is
// PURE and the record may be rebuilt on every tick -- that keeps per-tick
// context (host values, __now, lifted locals) fresh inside the closures.
//
// Per imported block there is exactly ONE static member; its TypeFighter
// type lives in `Builtins`. Signatures stay free of LocSta types so the
// generated C# only ever references this assembly.
namespace PureState.TypeFighter.Web

open System
open System.Collections.Generic
open LocSta.Core

module private LocStaRtImpl =

    let SF_init = PureState.TypeFighter.BuiltinTypes.StepFields.init
    let SF_step = PureState.TypeFighter.BuiltinTypes.StepFields.step
    let SF_value = PureState.TypeFighter.BuiltinTypes.StepFields.value
    let SF_next = PureState.TypeFighter.BuiltinTypes.StepFields.nextState

    let record (init: obj) (step: obj -> obj) : obj =
        let d = Dictionary<string, obj>()
        d.[SF_init] <- init
        d.[SF_step] <- box (Func<obj, obj>(step))
        box d

    let result (v: obj) (next: obj) : obj =
        let d = Dictionary<string, obj>()
        d.[SF_value] <- v
        d.[SF_next] <- next
        box d

    let stepOf (record: obj) : Func<obj, obj> =
        (record :?> IDictionary<string, obj>).[SF_step] :?> Func<obj, obj>

    let initOf (record: obj) : obj =
        (record :?> IDictionary<string, obj>).[SF_init]

    let lastOr (fallback: obj) (vs: seq<'v>) : obj =
        let mutable r = fallback
        for v in vs do r <- box v
        r

open LocStaRtImpl

[<AbstractClass; Sealed>]
type LocStaRt private () =

    // ─── Generic drivers ──────────────────────────────────────────────

    /// Drive a generator (no per-tick input). State = its StateController.
    static member Generator (sg: SigStream<'v, unit, 's>) : obj =
        record (box (StateController<'s>(ValueNone))) (fun st ->
            let ctrl = st :?> StateController<'s>
            let vs, _ = sg ctrl ()
            result (lastOr (box Unchecked.defaultof<'v>) vs) st)

    /// Drive a block over ONE input stream (a protocol record) feeding its
    /// LocSta context. Composed state = [| inputState; controller |].
    static member Filter1 (input: obj, sg: SigStream<'v, 'c, 's>) : obj =
        record (box [| initOf input; box (StateController<'s>(ValueNone)) |]) (fun st ->
            let arr = st :?> obj[]
            let r1 = (stepOf input).Invoke(arr.[0]) :?> IDictionary<string, obj>
            let ctrl = arr.[1] :?> StateController<'s>
            let vs, _ = sg ctrl (r1.[SF_value] :?> 'c)
            result (lastOr (box Unchecked.defaultof<'v>) vs) (box [| r1.[SF_next]; arr.[1] |]))

    /// Same over TWO input streams (LocSta context = a pair).
    static member Filter2 (in1: obj, in2: obj, sg: SigStream<'v, 'c1 * 'c2, 's>) : obj =
        record (box [| initOf in1; initOf in2; box (StateController<'s>(ValueNone)) |]) (fun st ->
            let arr = st :?> obj[]
            let r1 = (stepOf in1).Invoke(arr.[0]) :?> IDictionary<string, obj>
            let r2 = (stepOf in2).Invoke(arr.[1]) :?> IDictionary<string, obj>
            let ctrl = arr.[2] :?> StateController<'s>
            let vs, _ = sg ctrl (r1.[SF_value] :?> 'c1, r2.[SF_value] :?> 'c2)
            result (lastOr (box Unchecked.defaultof<'v>) vs) (box [| r1.[SF_next]; r2.[SF_next]; arr.[2] |]))

    /// Same over THREE input streams (LocSta context = a triple).
    static member Filter3 (in1: obj, in2: obj, in3: obj, sg: SigStream<'v, 'c1 * 'c2 * 'c3, 's>) : obj =
        record
            (box [| initOf in1; initOf in2; initOf in3; box (StateController<'s>(ValueNone)) |])
            (fun st ->
                let arr = st :?> obj[]
                let r1 = (stepOf in1).Invoke(arr.[0]) :?> IDictionary<string, obj>
                let r2 = (stepOf in2).Invoke(arr.[1]) :?> IDictionary<string, obj>
                let r3 = (stepOf in3).Invoke(arr.[2]) :?> IDictionary<string, obj>
                let ctrl = arr.[3] :?> StateController<'s>
                let vs, _ = sg ctrl (r1.[SF_value] :?> 'c1, r2.[SF_value] :?> 'c2, r3.[SF_value] :?> 'c3)
                result
                    (lastOr (box Unchecked.defaultof<'v>) vs)
                    (box [| r1.[SF_next]; r2.[SF_next]; r3.[SF_next]; arr.[3] |]))

    // ─── Imported blocks ──────────────────────────────────────────────

    // Statistics

    static member Ema (src: obj, alpha: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Statistics.Ema.ema alpha)

    static member MovingAverage (src: obj, n: double) : obj =
        // LocSta movingAverage is int-typed; truncate TF doubles into it.
        let sg = LocSta.Blocks.Statistics.MovingAverage.movingAverage (int n)
        LocStaRt.Filter1(src, fun s (c: float) -> sg s (int c))

    static member RollingStdDev (src: obj, n: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Statistics.RollingStdDev.rollingStdDev (int n))

    static member RollingMin (src: obj, n: double) : obj =
        let sg : SigStream<float, float, _> = LocSta.Blocks.Statistics.RollingMin.rollingMin (int n)
        LocStaRt.Filter1(src, sg)

    static member RollingMax (src: obj, n: double) : obj =
        let sg : SigStream<float, float, _> = LocSta.Blocks.Statistics.RollingMax.rollingMax (int n)
        LocStaRt.Filter1(src, sg)

    static member RunningMin (src: obj) : obj =
        let sg : SigStream<float, float, _> = LocSta.Blocks.Statistics.RunningMin.runningMin ()
        LocStaRt.Filter1(src, sg)

    static member RunningMax (src: obj) : obj =
        let sg : SigStream<float, float, _> = LocSta.Blocks.Statistics.RunningMax.runningMax ()
        LocStaRt.Filter1(src, sg)

    // Arithmetic

    static member Diff (src: obj, seed: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Arithmetic.Diff.diff seed)

    static member CumulativeSum (src: obj, seed: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Arithmetic.CumulativeSum.cumulativeSum seed)

    static member CumulativeProduct (src: obj, seed: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Arithmetic.CumulativeProduct.cumulativeProduct seed)

    static member Clamp (src: obj, lo: double, hi: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Arithmetic.Clamp.clamp lo hi)

    static member Rescale (src: obj, inLo: double, inHi: double, outLo: double, outHi: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Arithmetic.Rescale.rescale inLo inHi outLo outHi)

    // Delay

    static member DelayBy1 (src: obj, defaultValue: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Delay.DelayBy1.delayBy1 defaultValue)

    static member DelayByN (src: obj, n: double, defaultValue: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Delay.DelayByN.delayByN (int n) defaultValue)

    // State

    static member Hold (src: obj, pred: Func<obj, obj>, defaultValue: double) : obj =
        let p (v: float) = pred.Invoke(box v) :?> bool
        LocStaRt.Filter1(src, LocSta.Blocks.State.Hold.hold p defaultValue)

    static member Latch (trigger: obj, src: obj, defaultValue: double) : obj =
        LocStaRt.Filter2(trigger, src, LocSta.Blocks.State.Latch.latch defaultValue)

    static member Edge (trigger: obj) : obj =
        LocStaRt.Filter1(trigger, map double (LocSta.Blocks.State.Edge.edge ()))

    static member Changed (src: obj) : obj =
        let sg : SigStream<bool, float, _> = LocSta.Blocks.State.Changed.changed ()
        LocStaRt.Filter1(src, sg)

    // Logic

    static member Debounce (trigger: obj, n: double) : obj =
        LocStaRt.Filter1(trigger, LocSta.Blocks.Logic.Debounce.debounce (int n))

    static member Toggle (trigger: obj) : obj =
        LocStaRt.Filter1(trigger, LocSta.Blocks.Logic.Toggle.toggle ())

    // Counting

    static member CountWhere (src: obj, pred: Func<obj, obj>) : obj =
        let p (v: float) = pred.Invoke(box v) :?> bool
        LocStaRt.Filter1(src, map double (LocSta.Blocks.Counting.CountWhere.countWhere p))

    static member CountSince (trigger: obj) : obj =
        LocStaRt.Filter1(trigger, map double (LocSta.Blocks.Counting.CountSince.countSince ()))

    static member CountIn (trigger: obj, n: double) : obj =
        LocStaRt.Filter1(trigger, map double (LocSta.Blocks.Counting.CountIn.countIn (int n)))

    static member Rate (trigger: obj, n: double) : obj =
        LocStaRt.Filter1(trigger, LocSta.Blocks.Counting.Rate.rate (int n))

    // Detection

    static member Crossover (a: obj, b: obj) : obj =
        let projA : SigStream<float, float * float, unit> = fun s (x, _) -> [| x |] :> seq<_>, s
        let projB : SigStream<float, float * float, unit> = fun s (_, y) -> [| y |] :> seq<_>, s
        LocStaRt.Filter2(a, b, map double (LocSta.Blocks.Detection.Crossover.crossover projA projB))

    static member Threshold (src: obj, level: double) : obj =
        LocStaRt.Filter1(src, map double (LocSta.Blocks.Detection.Threshold.threshold level))

    // Windowing

    static member WindowedReduce (src: obj, n: double, f: Func<obj, obj>, seed: double) : obj =
        let folder (acc: float) (v: float) =
            (f.Invoke(box acc) :?> Func<obj, obj>).Invoke(box v) :?> float
        LocStaRt.Filter1(src, LocSta.Blocks.Windowing.WindowedReduce.windowedReduce (int n) folder seed)

    // Generators

    static member Counter (start: double, increment: double) : obj =
        // LocSta's counter is int-typed; TF numbers are doubles.
        LocStaRt.Generator(map double (LocSta.Blocks.Generators.Counter.counter (int start) (int increment)))

    static member Fibonacci () : obj =
        LocStaRt.Generator(map double (LocSta.Blocks.Generators.Fibonacci.fibonacci ()))

    static member WhiteNoise (seed: double) : obj =
        LocStaRt.Generator(LocSta.Blocks.Dsp.Oscillators.WhiteNoise.whiteNoise (int seed))

    // DSP oscillators

    static member SineOsc (freq: obj, sampleRate: double) : obj =
        LocStaRt.Filter1(freq, LocSta.Blocks.Dsp.Oscillators.SineOsc.sineOsc sampleRate)

    static member SawOsc (freq: obj, sampleRate: double) : obj =
        LocStaRt.Filter1(freq, LocSta.Blocks.Dsp.Oscillators.SawOsc.sawOsc sampleRate)

    static member SquareOsc (freq: obj, pulseWidth: obj, sampleRate: double) : obj =
        LocStaRt.Filter2(freq, pulseWidth, LocSta.Blocks.Dsp.Oscillators.SquareOsc.squareOsc sampleRate)

    static member TriangleOsc (freq: obj, sampleRate: double) : obj =
        LocStaRt.Filter1(freq, LocSta.Blocks.Dsp.Oscillators.TriangleOsc.triangleOsc sampleRate)

    // DSP filters (cutoff/center/q are modulatable input streams)

    static member LowPass1 (src: obj, cutoff: obj, sampleRate: double) : obj =
        LocStaRt.Filter2(src, cutoff, LocSta.Blocks.Dsp.Filters.LowPass1.lowPass1 sampleRate)

    static member HighPass1 (src: obj, cutoff: obj, sampleRate: double) : obj =
        LocStaRt.Filter2(src, cutoff, LocSta.Blocks.Dsp.Filters.HighPass1.highPass1 sampleRate)

    static member BiquadLowPass (src: obj, cutoff: obj, q: obj, sampleRate: double) : obj =
        LocStaRt.Filter3(src, cutoff, q, LocSta.Blocks.Dsp.Filters.BiquadLowPass.biquadLowPass sampleRate)

    static member BiquadHighPass (src: obj, cutoff: obj, q: obj, sampleRate: double) : obj =
        LocStaRt.Filter3(src, cutoff, q, LocSta.Blocks.Dsp.Filters.BiquadHighPass.biquadHighPass sampleRate)

    static member BiquadBandPass (src: obj, center: obj, q: obj, sampleRate: double) : obj =
        LocStaRt.Filter3(src, center, q, LocSta.Blocks.Dsp.Filters.BiquadBandPass.biquadBandPass sampleRate)

    static member DcBlock (src: obj, r: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Filters.DcBlock.dcBlock r)

    // DSP envelope

    static member EnvFollow (src: obj, sampleRate: double, attackMs: double, releaseMs: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Envelope.EnvFollow.envFollow sampleRate attackMs releaseMs)

    static member Adsr (gate: obj, attack: double, decay: double, sustain: double, release: double) : obj =
        LocStaRt.Filter1(gate, LocSta.Blocks.Dsp.Envelope.Adsr.adsr attack decay sustain release)

    // DSP dynamics

    static member SoftClip (src: obj, drive: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Dynamics.SoftClip.softClip drive)

    static member HardClip (src: obj, threshold: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Dynamics.HardClip.hardClip threshold)

    static member Gate (src: obj, envelope: obj, threshold: double) : obj =
        LocStaRt.Filter2(src, envelope, LocSta.Blocks.Dsp.Dynamics.Gate.gate threshold)

    // DSP modulation

    static member RingMod (a: obj, b: obj) : obj =
        let projA : SigStream<float, float * float, unit> = fun s (x, _) -> [| x |] :> seq<_>, s
        let projB : SigStream<float, float * float, unit> = fun s (_, y) -> [| y |] :> seq<_>, s
        LocStaRt.Filter2(a, b, LocSta.Blocks.Dsp.Modulation.RingMod.ringMod projA projB)

    static member BitCrush (src: obj, bits: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Modulation.BitCrush.bitCrush (int bits))

    static member Crossfade (a: obj, b: obj, mix: obj) : obj =
        let projA : SigStream<float, float * float * float, unit> = fun s (x, _, _) -> [| x |] :> seq<_>, s
        let projB : SigStream<float, float * float * float, unit> = fun s (_, y, _) -> [| y |] :> seq<_>, s
        let projM : SigStream<float, float * float * float, unit> = fun s (_, _, m) -> [| m |] :> seq<_>, s
        LocStaRt.Filter3(a, b, mix, LocSta.Blocks.Dsp.Modulation.Crossfade.crossfade projA projB projM)

    // DSP analysis

    static member Rms (src: obj, n: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Analysis.Rms.rms (int n))

    static member ZeroCrossRate (src: obj, n: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Analysis.ZeroCrossRate.zeroCrossRate (int n))

    static member PeakHold (src: obj, decay: double) : obj =
        LocStaRt.Filter1(src, LocSta.Blocks.Dsp.Analysis.PeakHold.peakHold decay)
