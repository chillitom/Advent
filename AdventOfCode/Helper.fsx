
open System.IO
open System.Text.RegularExpressions


let loadInput name = 
    let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "Input", name)
    File.ReadAllText(inputFile)

let loadLines name = 
    let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "Input", name)
    File.ReadAllLines(inputFile)

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success 
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None