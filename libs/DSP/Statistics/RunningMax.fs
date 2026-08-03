module DSP.Blocks.Statistics.RunningMax

open DSP.Core

/// All-time maximum: tracks the highest value ever seen.
let inline runningMax () =
    stream {
        let! ctx = getCtx()
        let! best = useStateWith (fun () -> ctx)
        if ctx > best.Value then best.Value <- ctx
        return best.Value
    }
