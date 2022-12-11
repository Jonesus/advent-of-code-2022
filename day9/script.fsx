open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n"

let parseInstruction (line: string) =
    let parts = line.Split " "
    Array.create (int parts[1]) parts[0]

let instructions = lines |> Array.collect parseInstruction

let headPosition = (0,0)
let tailPosition = (0,0)

let moveDown (x: int, y: int) = (x, y-1)
let moveUp (x: int, y: int) = (x, y+1)
let moveRight (x: int, y: int) = (x+1, y)
let moveLeft (x: int, y: int) = (x-1, y)

let moveHead (coordinates: int*int) (instruction: string) =
    match instruction with
    | "D" -> moveDown coordinates
    | "U" -> moveUp coordinates
    | "R" -> moveRight coordinates
    | "L" -> moveLeft coordinates
    | _ -> raise (new System.NotImplementedException("fda"))

let needToMove (xd: int, yd: int) =
    match xd, yd with
    | _, yd  when abs yd > 1 -> true
    | xd, _  when abs xd > 1 -> true
    | _                      -> false

let followTail (headCoordinates: int*int) (tailCoordinates: int*int) =
    let xDiff = fst headCoordinates - fst tailCoordinates
    let yDiff = snd headCoordinates - snd tailCoordinates

    if needToMove (xDiff, yDiff)
    then
        match (xDiff, yDiff) with
        | (0, y) when y < 0         -> moveDown tailCoordinates
        | (0, y) when y > 0         -> moveUp tailCoordinates
        | (x, 0) when x < 0         -> moveLeft tailCoordinates
        | (x, 0) when x > 0         -> moveRight tailCoordinates
        | (x, y) when x>0 && y>1    -> (moveRight >> moveUp) tailCoordinates
        | (x, y) when x<0 && y>1    -> (moveLeft >> moveUp) tailCoordinates
        | (x, y) when x>0 && y < -1 -> (moveRight >> moveDown) tailCoordinates
        | (x, y) when x<0 && y < -1 -> (moveLeft >> moveDown) tailCoordinates
        | (x, y) when y>0 && x>1    -> (moveRight >> moveUp) tailCoordinates
        | (x, y) when y<0 && x>1    -> (moveRight >> moveDown) tailCoordinates
        | (x, y) when y>0 && x < -1 -> (moveLeft >> moveUp) tailCoordinates
        | (x, y) when y<0 && x < -1 -> (moveLeft >> moveDown) tailCoordinates
        | a ->
            printfn "err: %A" a
            raise (new System.NotImplementedException("fda"))
    else tailCoordinates


let mutable positions = Set.empty.Add tailPosition
    
let iterate (hc: int*int, tc: int*int) (instruction: string) =
    let newHead = moveHead hc instruction
    let newTail = followTail newHead tc
    positions <- positions.Add newTail
    (newHead, newTail)

instructions |> Array.fold iterate (headPosition,tailPosition)

let results = positions.Count

printfn "positions: %A" results


let tails = Array.create 9 (0,0) |> List.ofArray
let mutable multiPositions = Set.empty.Add tailPosition

let rec iterateTail (tails: list<int*int>) =
    match tails with
    | th :: tt :: rest -> th :: iterateTail ((followTail th tt) :: rest)
    | th :: [] -> [th]
    | [] -> []


let multiIterate (hc: int*int, tails: list<int*int>) (instruction: string) =
    let newHead = moveHead hc instruction
    let newTails = iterateTail (newHead :: tails) |> List.tail
    multiPositions <- multiPositions.Add (List.last newTails)
    (newHead, newTails)

instructions |> Array.fold multiIterate (headPosition,tails)

let newResults = multiPositions.Count

printfn "new positions: %A" newResults

