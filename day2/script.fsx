open System.IO

let lines = File.ReadAllText("input.txt").Split "\n"

type RPS = Rock | Paper | Scissors

let choice a =
    match a with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> raise (new System.NotImplementedException())

let stringToChoices (a: string) =
    let pairs = a.Split " "
    match pairs with
        | [| a ; b |] -> (choice a, choice b)
        | _ -> raise (new System.NotImplementedException())

let choiceScore a =
    match a with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let outcomeScore = function
    | (a, b) when a = b -> 3
    | (a, b) when a = Rock && b = Paper -> 6
    | (a, b) when a = Paper && b = Scissors -> 6
    | (a, b) when a = Scissors && b = Rock -> 6
    | _ -> 0

let score =
    lines
    |> Array.map stringToChoices
    |> Array.map (fun (a,b) -> ((choiceScore b) + (outcomeScore (a, b))))
    |> Array.sum

printfn "score: %A" score

type WLD = Win | Loss | Draw

let target a =
    match a with
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> raise (new System.NotImplementedException())

let ownChoice = function
    | (opponent, target) when target = Draw -> opponent
    | (opponent, target) when opponent = Rock && target = Win -> Paper
    | (opponent, target) when opponent = Rock && target = Loss -> Scissors
    | (opponent, target) when opponent = Paper && target = Win -> Scissors
    | (opponent, target) when opponent = Paper && target = Loss -> Rock
    | (opponent, target) when opponent = Scissors && target = Win -> Rock
    | (opponent, target) when opponent = Scissors && target = Loss -> Paper
    | _ -> raise (new System.NotImplementedException())

let stringWithTargetToChoices (a: string) =
    let pairs = a.Split " "
    match pairs with
        | [| a ; b |] -> (choice a, ownChoice (choice a, target b))
        | _ -> raise (new System.NotImplementedException())

let newScore =
    lines
    |> Array.map stringWithTargetToChoices
    |> Array.map (fun (a,b) -> ((choiceScore b) + (outcomeScore (a, b))))
    |> Array.sum

printfn "new score: %A" newScore
