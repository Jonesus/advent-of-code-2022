open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n"

let parseLine (line: string) =
    let parts = line.Split " "
    match parts[0] with
    | "addx" -> (2, int parts[1])
    | _      -> (1, 0)

let initialX = 1

let instructions =
    lines
    |> Array.map parseLine
    |> Array.collect (fun (c, x) -> if c = 2 then [|(1,0); (1,0); (0,x)|] else [|(1,0)|])

let mutable cycles = 0

let whileCycleIsLessThan (value: int) (c: int, x: int) =
    cycles <- cycles + c
    cycles < value

let sumForCheckpoint (checkpoint: int) =
    let sum =
        instructions
        |> Array.takeWhile (whileCycleIsLessThan checkpoint)
        |> Array.map (fun (_, x) -> x)
        |> Array.sum
        |> ( + ) 1
        |> ( * ) checkpoint
    cycles <- 0
    sum
    
let checkpoints = [20 .. 40 .. 220]

let fullCount =
    checkpoints
    |> List.map sumForCheckpoint
    |> List.sum

printfn "full count: %A" fullCount



let foldRegister (currentX: int, list: list<int>) (c: int, x: int) =
    match c with
    | 1 -> (currentX, list @ [currentX])
    | _ -> (currentX + x, list)

let _, register =
    instructions
    |> Array.fold foldRegister (0, [])

let drawer (i: int) (regval: int) =
    if [regval .. regval+2] |> List.contains (i % 40)
    then '#'
    else '.'

register
|> List.mapi drawer
|> List.chunkBySize 40
|> List.map System.String.Concat
|> List.map (printfn "%A")
