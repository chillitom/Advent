#I __SOURCE_DIRECTORY__
#load "Helper.fsx"

open Helper
open System

let input = loadLines "level7.txt"

(*
--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates! Unfortunately, little Bobby is a little under the recommended age range, and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal (a number from 0 to 65535). A signal is provided to each wire by a gate, another wire, or some specific value. Each wire can only get a signal from one source, but can provide its signal to multiple destinations. A gate provides no signal until all of its inputs have a signal.

The included instructions booklet describes how to connect the parts together: x AND y -> z means to connect wires x and y to an AND gate, and then connect its output to wire z.

For example:

123 -> x means that the signal 123 is provided to wire x.
x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.
Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some reason, you'd like to emulate the circuit instead, almost all programming languages (for example, C, JavaScript, or Python) provide operators for these gates.

For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456
In little Bobby's kit's instructions booklet (provided as your puzzle input), what signal is ultimately provided to wire a?

*)

type WireRef = WireRef of string | Constant of int

type Signal = WireRef * Circuit

and Circuit =
    | Ref of WireRef
    | And of WireRef * WireRef
    | Or of WireRef * WireRef
    | LShift of WireRef * int
    | RShift of WireRef * int
    | Not of WireRef


let parse line = 
    match line with
    | Regex "^(\d+) -> ([a-z]+)" [constant; dest] -> (dest, Ref(Constant(int(constant))))
    | Regex "^([a-z]+) -> ([a-z]+)" [wire; dest] -> (dest, Ref(WireRef(wire)))
    | Regex "([a-z]+) AND ([a-z]+) -> ([a-z]+)" [wire1; wire2; dest] -> (dest, And(WireRef(wire1), WireRef(wire2)))
    | Regex "(\d+) AND ([a-z]+) -> ([a-z]+)" [signal; wire; dest] -> (dest, And(Constant(int(signal)), WireRef(wire)))
    | Regex "([a-z]+) OR ([a-z]+) -> ([a-z]+)" [wire1; wire2; dest] -> (dest, Or(WireRef(wire1), WireRef(wire2)))
    | Regex "([a-z]+) LSHIFT (\d+) -> ([a-z]+)" [wire1; bits; dest] -> (dest, LShift(WireRef(wire1),int(bits)))
    | Regex "([a-z]+) RSHIFT (\d+) -> ([a-z]+)" [wire1; bits; dest] -> (dest, RShift(WireRef(wire1),int(bits)))
    | Regex "NOT ([a-z]+) -> ([a-z]+)" [wire; dest] -> (dest, Not(WireRef(wire)))
    | _ -> failwith ("Couldn't parse: " + line)

let wireList = input |> Seq.map parse |> Seq.toList

let wires = wireList |> Map.ofList

let mutable memo = Map.empty

let rec value (wires:Map<string, Circuit>) path wireref : int = 

    let get = value wires (wireref :: path)

    printfn "%d" (List.length path)

    match wireref with
    | Constant i ->
        printfn "const %d" i 
        i
    | WireRef s ->
        match Map.tryFind s memo with
        | Some i -> i
        | _ -> 
            let circuit = wires.[s]
            printfn "%s -> %A" s circuit
            let value =         
                match circuit with
                    | Ref w -> get w
                    | And (w1,w2) -> (get w1) &&& (get w2) 
                    | Or (w1,w2) -> (get w1) ||| (get w2)
                    | LShift (w,b) -> (get w) <<< b
                    | RShift (w,b) -> (get w) >>> b
                    | Not(w) -> ~~~ (get w)
            memo <- Map.add s value memo 
            value


let answer1 = value wires [] (WireRef("a"))

(*
--- Part Two ---

Now, take the signal you got on wire a, override wire b to that signal, and reset the other wires (including wire a). 

What new signal is ultimately provided to wire a?
*)

let wires2 = Map.add "b" (Ref(Constant(3176))) wires

memo <- Map.empty

let answer2 = value wires2 [] (WireRef("a"))

