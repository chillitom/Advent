#load "Helper.fsx"

open Helper
open System
open System.Text.RegularExpressions

let input = loadLines "level6.txt"

(*
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

For example:

turn on 0,0 through 999,999 would turn on (or leave on) every light.
toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
After following the instructions, how many lights are lit?
*)

let row i = [0..999] |> Seq.map (fun _ -> false)
let lights = [0..999] |> Seq.map row

let turnOn x = true
let turnOff x = false
let toggle x = not x

let parse i =
    let m = Regex.Match(i, @"(?<instruction>turn on|toggle|turn off) (?<x1>\d+),(?<y1>\d+) through (?<x2>\d+),(?<y2>\d+)")
    let get (s:string) = m.Groups.[s].Value |> Convert.ToInt32
    let instruction = 
        match m.Groups.["instruction"].Value with
        | "turn on" -> turnOn
        | "turn off" -> turnOff
        | "toggle" -> toggle
        | _ -> failwith i
    (instruction, get "x1", get "y1", get "x2", get "y2")

let instructions = input |> Seq.map parse

let updateRow (f,x1,_,x2,_) row =
    row |> Seq.mapi (fun x v -> if x >= x1 && x <= x2 then f v else v)

let updateLights lights ((_,_,y1,_,y2) as i) =
    lights |> Seq.mapi (fun y r -> if y >= y1 && y <= y2 then updateRow i r else r)

let lightsAfterUpdate =
    instructions |> Seq.fold updateLights lights

let answer1 = lightsAfterUpdate |> Seq.collect id |> Seq.filter id |> Seq.length

(*
--- Part Two ---

You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of those lights by 1.

The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of those lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?

For example:

turn on 0,0 through 0,0 would increase the total brightness by 1.
toggle 0,0 through 999,999 would increase the total brightness by 2000000.

*)