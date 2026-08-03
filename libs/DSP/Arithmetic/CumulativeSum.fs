module DSP.Blocks.Arithmetic.CumulativeSum

open DSP.Core

/// Running total: accumulates the sum of all input values starting from 'seed'.
let inline cumulativeSum seed =
    stream {
        let! ctx = getCtx()
        let! sum = useState seed
        sum.Value <- sum.Value + ctx
        return sum.Value
    }
