open System.IO

let lines = File.ReadAllText("input.txt")

let groups =
    lines.Split "\n\n"
    |> Array.map (fun g -> g.Split "\n" |> Array.map int |> Array.sum)

printfn "max: %A" (Array.max groups)

let top3 = Array.sort groups |> Array.rev |> Array.take 3 |> Array.sum

printfn "top3 sum: %A" top3
