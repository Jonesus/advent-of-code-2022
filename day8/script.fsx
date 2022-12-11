open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n"

let cells =
    lines
    |> Array.map (Seq.toArray >> Array.map (fun x -> int x - int '0'))

let height = Array.length cells
let width = Array.length cells[0]

let newcells = Array2D.init width height (fun i j -> cells[i][j])

let isVisible i j v =
    if i = 0 || j = 0 || i+1 = height || j+1 = width
    then true
    else
        let allToRight = newcells[i, j+1..]
        let allToLeft = newcells[i, ..j-1]
        let allToDown = newcells[i+1.., j]
        let allToUp = newcells[..i-1, j]

        let lines = [allToRight; allToLeft; allToDown; allToUp]
        lines
        |> List.exists (fun a -> (a |> Array.forall (fun x -> x < v)))

let mutable sum = 0

let results =
    newcells
    |> Array2D.mapi isVisible
    |> Array2D.map (fun x -> if x then sum <- sum+1 else ())

printfn "visible count: %A" sum

let viewScoreInDirection (v: int) (arr: int[]) =
    arr
        |> Array.takeWhile  (fun x -> v > x)
        |> Array.length
        |> fun x -> if Array.length arr = x then x else x+1

let viewScore i j v =
    if i = 0 || j = 0 || i+1 = height || j+1 = width
    then 0
    else
        let allToRight = newcells[i, j+1..]
        let allToLeft = newcells[i, ..j-1] |> Array.rev
        let allToDown = newcells[i+1.., j]
        let allToUp = newcells[..i-1, j] |> Array.rev

        let lines = [allToRight; allToLeft; allToDown; allToUp]
        printfn "value: %A, line: %A, score: %A" v allToRight (viewScoreInDirection v allToRight)

        lines
        |> List.map (viewScoreInDirection v)
        |> List.fold (*) 1


let mutable highscore = 0

let secondresults =
    newcells
    |> Array2D.mapi viewScore
    |> Array2D.map (fun x -> if x > highscore then highscore <- x else ())


printfn "%A\n\n" (newcells)
printfn "%A" (newcells |> Array2D.mapi viewScore)

printfn "highscore: %A" highscore
