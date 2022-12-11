open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n"

let cutToTwo list =
    let length = Array.length list / 2
    let firstHalf = Array.take length list
    let secondHalf = Array.skip length list
    (firstHalf, secondHalf)

let intersection (a, b) =
    Set.intersect (Set.ofArray a) (Set.ofArray b) |> Set.toArray |> Array.head

let priority a =
    if System.Char.IsLower a
    then int a - int 'a' + 1
    else int a - int 'A' + 27

let result =
    lines
    |> Array.map (Seq.toArray >> cutToTwo >> intersection >> priority)
    |> Array.sum

printfn "priority sum: %A" result

let intersectMany xs =
    xs
    |> Array.map (Seq.toArray >> Set.ofArray)
    |> Set.intersectMany
    |> Set.toArray
    |> Array.head

let newResult =
    lines
    |> Array.chunkBySize 3
    |> Array.map (intersectMany >> priority)
    |> Array.sum

printfn "new priority sum: %A" newResult
