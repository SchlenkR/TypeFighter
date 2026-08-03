module DSP.Blocks.Dsp.Modulation.RingMod

open DSP.Core

/// Ring modulator: multiplies two signals.
let inline ringMod s1 s2 =
    stream {
        let! v1 = s1
        let! v2 = s2
        return v1 * v2
    }
