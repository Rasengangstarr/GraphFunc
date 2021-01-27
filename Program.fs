// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let sqrd (x : float) = x * x * x

let charsFunction (x : float) = x * 2.0

let valsForFunc (s, f, st : float, fnc) =
    seq { for x in s .. st .. f do yield (x, fnc x) }

let min x y =
    match snd x < snd y with
    | true -> x
    | false -> y

let max x y =
    match snd x > snd y with
    | true -> x
    | false -> y


let row (s : float, f : float, v : float) =
    seq { for x in floor s .. floor f do yield (if x = floor v then "X" else " ")}

let graph (s, f, st :float, fnc) =
    let vals = valsForFunc(s, f, st, fnc)
    let minY = snd (Seq.reduce min vals)
    let maxY = snd (Seq.reduce max vals)

    for i in seq { for y in s  .. st .. f do yield row (minY, maxY, (snd (Seq.find (fun z -> (fst z) = y) vals) )) }  do
        for j in i do
            printf "%s" j
        printf "\n"


[<EntryPoint>]
let main argv =
    graph (-5.0, 5.0, 0.1, (fun x -> 5.0 * sin (x*x)))
    0

//