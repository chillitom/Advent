#load "Helper.fsx"

open Helper
open System

let input = loadLines "level5.txt"

(*
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
For example:

ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
jchzalrnumimnmhp is naughty because it has no double letter.
haegwjzuvuyypxyu is naughty because it contains the string xy.
dvszwmarrgswjxmb is naughty because it contains only one vowel.
How many strings are nice?
*)

let containsThreeVowels s =
    (Seq.filter (fun c -> "aeiou".Contains(c.ToString())) s |> Seq.length) > 2

let containsRunOf2 s =
     Seq.pairwise s |> Seq.exists (fun (a,b) -> a = b)

let nothingForbidden s =
    let forbidden = ["ab"; "cd"; "pq"; "xy"]
    Seq.pairwise s |> (Seq.exists (fun (a,b) -> (Seq.contains (new String([|a;b|])) forbidden))) |> not


let isNice s = containsThreeVowels s && containsRunOf2 s && nothingForbidden s

let answer1 = Seq.filter isNice input |> Seq.length

isNice "ugknbfddgicrmopn"
containsRunOf2 "ugknbfddgicrmopn"
containsThreeVowels "ugknbfddgicrmopn"
nothingForbidden "ugknbfddgicrmopn"