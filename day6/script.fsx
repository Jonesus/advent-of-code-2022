open System.IO

let input = File.ReadAllText "input.txt"

let getIndex window =
    input
    |> Seq.toList
    |> (Seq.windowed window)
    |> Seq.findIndex (fun xs -> Set.count (Set.ofArray xs) = Array.length xs )
    |> (+) window

let result = getIndex 4

printfn "Found index: %A" result

Seq.windowed 4 (Seq.toList input)

let newResult = getIndex 14

printfn "Found new index: %A" newResult
