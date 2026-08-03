module DSP.Blocks.Arithmetic.Diff

open DSP.Core

/// Computes the difference between the current and the previous value.
let inline diff defaultValue =
    stream {
        let! ctx = getCtx()
        let! prev = useState defaultValue
        let output = ctx - prev.Value
        prev.Value <- ctx
        return output
    }
