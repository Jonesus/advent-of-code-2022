open System.IO
open System.Text.RegularExpressions

let pairs = (File.ReadAllText "input.txt").Split "\n\n"

let parseLine (line: string) =
    Regex.Matches(line, @"[[]|\d+|[]]") |> Seq.toList |> List.map (fun x -> x.Value)

type Elem =
    | Number of int
    | List of list<Elem>

let parseTokens (tokens: list<string>) =
    let mutable current = tokens
    let mutable stack: list<Elem> = []

    while current |> List.length > 1 do
        match List.head current with
        | "[" -> stack <- (List [] :: stack)
        | "]" ->
            match stack[1] with
            | List s1 -> stack <- [List (s1 @ [stack[0]])] @ (List.tail <| List.tail stack)
            | _   -> raise (new System.NotImplementedException("closing"))
        |  x  ->
            match stack[0] with
            | List s0 -> stack <- List (s0 @ [Number (int x)]) :: (List.tail stack)
            | _   -> raise (new System.NotImplementedException("adding"))
        current <- List.tail current
    stack[0]

let rec compareElems (left: Elem, right: Elem) =
    match left, right with
    | Number a, Number b -> if a = b then None else Some (a < b)
    | Number a, List b -> compareElems (List [Number a], List b)
    | List a, Number b -> compareElems (List a, List [Number b])
    | List a, List b ->
        match a, b with
        | [], [] -> None
        | [], bRest when List.length bRest > 0 -> Some true  
        | aRest, [] when List.length aRest > 0 -> Some false
        | aVal :: aRest, bVal :: bRest ->
            match compareElems (aVal, bVal) with
            | Some x -> Some x
            | None -> compareElems (List aRest, List bRest)
        | _   -> raise (new System.NotImplementedException("impossible"))

    
let result =
    pairs
    |> Array.map (fun x -> parseLine <| (x.Split "\n")[0], parseLine <| (x.Split "\n")[1])
    |> Array.map (fun x -> parseTokens <| (fst x), parseTokens <| snd x)
    |> Array.map compareElems
    |> Array.map (Option.defaultValue false)
    |> Array.indexed
    |> Array.fold (fun acc (i, b) -> if b then acc + i + 1 else acc) 0

printfn "sum: %A" result

let parsed =
    pairs
    |> Array.collect (fun x -> (x.Split "\n") |> Array.map parseLine)
    |> Array.map parseTokens

let key1 = List [List [Number 2]]
let key2 = List [List [Number 6]] 

let sorted =
    Array.append [|key1; key2 |] parsed
    |> Array.sortWith (fun a b -> if (compareElems (a,b)).Value then -1 else 1)

let firstIndex = sorted |> Array.findIndex (fun x -> x = key1)
let secondIndex = sorted |> Array.findIndex (fun x -> x = key2)

printfn "product: %A" ((firstIndex + 1) * (secondIndex + 1))
