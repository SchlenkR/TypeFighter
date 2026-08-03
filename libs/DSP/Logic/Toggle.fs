module DSP.Blocks.Logic.Toggle

open DSP.Core

/// Toggles between true/false each time the input is true.
let toggle () =
    stream {
        let! ctx = getCtx()
        let! state = useState false
        if ctx then state.Value <- not state.Value
        return state.Value
    }
