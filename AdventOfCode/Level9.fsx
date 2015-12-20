#I __SOURCE_DIRECTORY__
#load "Helper.fsx"

open Helper
open System
open System.Text.RegularExpressions

let input = loadLines "level9.txt"

(*
--- Day 9: All in a Single Night ---

Every year, Santa manages to deliver all of his presents in a single night.

This year, however, he has some new locations to visit; his elves have provided him the distances between every pair of locations. He can start and end at any two (different) locations he wants, but he must visit each location exactly once. What is the shortest distance he can travel to achieve this?

For example, given the following distances:

London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
The possible routes are therefore:

Dublin -> London -> Belfast = 982
London -> Dublin -> Belfast = 605
London -> Belfast -> Dublin = 659
Dublin -> Belfast -> London = 659
Belfast -> Dublin -> London = 605
Belfast -> London -> Dublin = 982
The shortest of these is London -> Dublin -> Belfast = 605, and so the answer is 605 in this example.

What is the distance of the shortest route?
*)

let parse = function
    | Regex "(\w+) to (\w+) = (\d+)" [a;b;d] -> [((a,b),int(d)); ((b,a),int(d))]
    | _ -> failwith "WAT!"

let edges = input |> Seq.collect parse |> Seq.toList

let distances = Map.ofList edges

let lookup route = Map.find route distances

let cities = edges |> Seq.map (fun ((x,_),_) -> x) |> Seq.distinct |> Seq.toList

let rec permutations list taken = 
  seq { if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations list (Set.add l taken)  do
              yield l::perm }

let routes = permutations cities Set.empty

let distance route = route |> Seq.pairwise |> Seq.map lookup |> Seq.sum

let answer1 = routes |> Seq.map distance |> Seq.min

(*
--- Part Two ---

The next year, just to show off, Santa decides to take the route with the longest distance instead.

He can still start and end at any two (different) locations he wants, and he still must visit each location exactly once.

For example, given the distances above, the longest route would be 982 via (for example) Dublin -> London -> Belfast.

What is the distance of the longest route?
*)

let answer2 = routes |> Seq.map distance |> Seq.max