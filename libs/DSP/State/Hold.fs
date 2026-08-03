module DSP.Blocks.State.Hold

open DSP.Core

/// Holds the last value that satisfies the predicate; returns 'defaultValue' until then.
let inline hold ([<InlineIfLambda>] predicate) defaultValue =
    stream {
        let! ctx = getCtx()
        let! held = useState defaultValue
        if predicate ctx then held.Value <- ctx
        return held.Value
    }
