module DSP.Blocks.Statistics.RollingMax

open DSP.Core

/// Maximum value within a sliding window of 'windowSize' samples.
let inline rollingMax windowSize =
    stream {
        let! ctx = getCtx()
        let! window = useState []
        let newWindow = (ctx :: window.Value) |> List.truncate windowSize
        window.Value <- newWindow
        return newWindow |> List.max
    }
