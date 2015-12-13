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

let makeGrid length height value =
    let row i = [0..(length-1)] |> Seq.map (fun _ -> value)
    [0..(height-1)] |> Seq.map row
 
let lights = makeGrid 1000 1000 false

type Grid = int * int * int * int
type Action = TurnOn | TurnOff | Toggle
type Instruction = Action * Grid

let parse i =
    let m = Regex.Match(i, @"(?<instruction>turn on|toggle|turn off) (?<x1>\d+),(?<y1>\d+) through (?<x2>\d+),(?<y2>\d+)")
    let get (s:string) = m.Groups.[s].Value |> Convert.ToInt32
    let grid = get "x1", get "y1", get "x2", get "y2"
    match m.Groups.["instruction"].Value with
    | "turn on" -> TurnOn, grid
    | "turn off" -> TurnOff, grid
    | "toggle" -> Toggle, grid
    | _ -> failwith i

let instructions = input |> Seq.map parse

let updateRow f (action,(x1,_,x2,_)) row =
    row |> Seq.mapi (fun x v -> if x >= x1 && x <= x2 then f action v else v)

let updateLights f lights ((_,(_,y1,_,y2)) as i) =
    lights |> Seq.mapi (fun y r -> if y >= y1 && y <= y2 then updateRow f i r else r)

let updateLight action value = 
    match action with
    | TurnOn -> true
    | TurnOff -> false
    | Toggle -> not value

let answer1 = 
    instructions 
    |> Seq.fold (updateLights updateLight) lights 
    |> Seq.collect id 
    |> Seq.filter id 
    |> Seq.length

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

let updateLight2 action value = 
    match action with
    | TurnOn -> value + 1
    | TurnOff -> max (value - 1) 0
    | Toggle -> value + 2

let lights2 = makeGrid 1000 1000 0

let answer2 = 
    instructions 
    |> Seq.fold (updateLights updateLight2) lights2 
    |> Seq.collect id 
    |> Seq.sum

