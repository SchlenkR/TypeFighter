module DSP.Blocks.Generators.Counter

open DSP.Core

/// Generates an incrementing counter starting at 'start' with given 'increment'.
let counter start increment =
    stream {
        let! state = useState start
        let current = state.Value
        state.Value <- current + increment
        return current
    }
