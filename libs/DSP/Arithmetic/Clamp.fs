module DSP.Blocks.Arithmetic.Clamp

open DSP.Core

/// Constrains the input value to the range [lo, hi].
let inline clamp lo hi =
    stream {
        let! ctx = getCtx()
        return ctx |> max lo |> min hi
    }
