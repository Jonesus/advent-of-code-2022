open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n"

let pairToRanges (x: string) =
    let endpoints = x.Split "-"
    let start = Array.head endpoints |> int
    let stop = Array.last endpoints |> int
    set [start .. stop]

let lineToRanges (x: string) =
    let ranges = x.Split ","
    (Array.head ranges |> pairToRanges, Array.last ranges |> pairToRanges)

let isOverlap (a,b) =
    Set.isSubset a b || Set.isSubset b a

let result =
    lines
    |> Array.map (lineToRanges >> isOverlap)
    |> Array.filter id
    |> Array.length

printfn "Full overlaps: %A" result

let isIntersect (a,b) =
    Set.intersect a b |> Set.count > 0

let newResult =
    lines
    |> Array.map (lineToRanges >> isIntersect)
    |> Array.filter id
    |> Array.length

printfn "Partial overlaps: %A" newResult
