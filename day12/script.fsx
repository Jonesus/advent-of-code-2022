open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n"
let data = lines |> Array.map Seq.toArray

let fixStartStop (a: char) =
    match a with
    | 'S' -> 'a'
    | 'E' -> 'z'
    |  x  ->  x

let canMove (start: char) (stop: char) =
    int (fixStartStop stop) - int (fixStartStop start) <= 1

let getNeighbors (canMove: char -> char -> bool) (i: int, j: int) (map: char array array) =
    let height = map |> Array.length
    let width = map |> (Array.head >> Array.length)
    let candidates = [i+1,j; i-1,j; i,j+1; i,j-1]

    candidates |> List.fold (fun acc pair ->
        match pair with
        | ci, _   when ci < 0 || ci >= height -> acc
        | _, cj   when cj < 0 || cj >= width  -> acc
        | ci, cj when canMove (map[i][j]) (map[ci][cj]) -> (ci,cj) :: acc
        | _ -> acc
        ) []

let rec path (map: Map<int*int, int*int>) (acc: list<int*int>) (node: int*int) =
    match map |> Map.find node with
    | (-1, -1) -> acc
    | x        -> path map (x :: acc) x

let coordinates = List.allPairs [0 .. (data |> Array.length) - 1] [0 .. (data |> (Array.head >> Array.length)) - 1]
let start = coordinates |> List.find (fun (i, j) -> data[i][j] = 'S')
let stop = coordinates |> List.find (fun (i, j) -> data[i][j] = 'E')

let mutable queue = [start]
let mutable cameFrom = Map.ofList [start, (-1,-1)]

while List.length queue > 0 do
    let current = List.head queue
    queue <- List.tail queue

    getNeighbors canMove current data
    |> List.iter (fun neighbor ->
        if Map.containsKey neighbor cameFrom
        then ()
        else
            queue <- List.append queue [neighbor]
            cameFrom <- Map.add neighbor current cameFrom
        )

printfn "res: %A" cameFrom

printfn "path: %A" (path cameFrom [] stop)
printfn "length: %A" (List.length (path cameFrom [] stop))


let newCanMove (start: char) (stop: char) =
    int (fixStartStop start) - int (fixStartStop stop) <= 1

let mutable newQueue = [stop]
let mutable newCameFrom = Map.ofList [stop, (-1,-1)]

while List.length newQueue > 0 do
    let current = List.head newQueue
    newQueue <- List.tail newQueue

    getNeighbors newCanMove current data
    |> List.iter (fun neighbor ->
        if Map.containsKey neighbor newCameFrom
        then ()
        else
            newQueue <- List.append newQueue [neighbor]
            newCameFrom <- Map.add neighbor current newCameFrom
        )

let possibilities =
    newCameFrom
    |> Map.keys
    |> Seq.filter (fun (i,j) -> data[i][j] = 'a')

let bestLength =
    possibilities
    |> Seq.map (fun x -> List.length (path newCameFrom [] x))
    |> Seq.sort
    |> Seq.head

printfn "best length: %A" bestLength
