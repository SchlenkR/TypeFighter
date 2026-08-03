module DSP.Blocks.Dsp.Dynamics.HardClip

open DSP.Core

/// Hard clipper / limiter. Clips signal to [-threshold, +threshold].
let inline hardClip threshold =
    stream {
        let! signal = getCtx()
        return signal |> max (-threshold) |> min threshold
    }
