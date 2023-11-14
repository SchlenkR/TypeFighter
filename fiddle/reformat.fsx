
open System.IO

let file = File.ReadAllLines "../TypeFighter/Core.fs"

let res =
  [| for line in file do
      let trimmed = line.TrimStart()
      let spacesCount = line.Length - trimmed.Length
      let newSpaces = String.replicate (spacesCount / 2) " "
      yield newSpaces + trimmed
  |]

File.WriteAllLines("../TypeFighter/CoreNew.fs", res)
