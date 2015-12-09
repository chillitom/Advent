
open System.IO

let loadInput name = 
    let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "Input", name)
    File.ReadAllText(inputFile)

let loadLines name = 
    let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "Input", name)
    File.ReadAllLines(inputFile)