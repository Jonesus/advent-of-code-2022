open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n\n"

type Monkey = {
    items: bigint[];
    operation: bigint -> bigint;
    test: bigint -> int;
    divisor: int;
}

let parseMonkey (rows: string[]) =
    let items = (rows[1].Split(": ")[1]).Split(", ") |> Array.map (int >> bigint)

    let opParts = (rows[2].Split("= ")[1]).Split(" ")
    let operator =
        match opParts[1] with
        | "*" -> ( * )
        | "+" -> ( + )
        | x   -> raise (new System.NotImplementedException("incorrect op: " + x))
    let operation = fun (x: bigint) ->
        operator x (match opParts[2] with
                    | "old" -> x
                    | a     -> a |> int |> bigint)

    let divisor = rows[3].Split " " |> Array.last |> int |> bigint
    let trueTarget = rows[4].Split " " |> Array.last |> int
    let falseTarget = rows[5].Split " " |> Array.last |> int

    let test = fun (x: bigint) ->
        if x % divisor = bigint 0
        then trueTarget
        else falseTarget

    { items=items; operation=operation; test=test; divisor=int divisor }
    
let updateMonkey (i: int) (f: Monkey -> Monkey) (all: Map<int,Monkey>) =
    all |> Map.change i (fun x ->
                         match x with
                         | Some m -> Some (f m)
                         | None   -> None)

let catch (item: bigint) (m: Monkey) =
    { m with items = Array.append m.items [| item |]}

let iterate (all: Map<int,Monkey>, inspections: Map<int, bigint>) (index: int) =
    let m = Map.find index all
    let inspected = m.items |> Array.map m.operation |> Array.map (fun x -> x / (bigint 3))
    let currentMap = updateMonkey index (fun m -> { m with items = [||] }) all

    let updatedMap =
        inspected
        |> Array.fold (fun curr i -> updateMonkey (m.test i) (catch i) curr) currentMap

    let updatedInspections = inspections |> Map.change index (fun x ->
        match x with
        | Some v -> Some (v + bigint (Array.length inspected))
        | None   -> None)

    (updatedMap, updatedInspections)

let score (m: Map<int,bigint>)=
    m
    |> Map.values
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold ( * ) (bigint 1)


let monkeys =
    lines
    |> Array.map (fun x -> x.Split "\n")
    |> Array.map parseMonkey
    |> Array.mapi (fun i m -> (i, m))
    |> Map.ofArray

let monkeyCount = monkeys |> Map.count
let inspections = Array.init monkeyCount (fun x -> (x,bigint 0)) |> Map.ofArray

let _, results =
    [1..20]
    |> List.fold (fun current _ ->
        monkeys
        |> Map.keys
        |> Seq.fold iterate current) (monkeys, inspections)

printfn "score: %A" (score results)




let rec gcd a b =
    match (a,b) with
    | (x,y) when x = y -> x
    | (x,y) when x > y -> gcd (x-y) y
    | (x,y)            -> gcd x (y-x)

let lcm a b = a*b/(gcd a b)

let divisors =
    monkeys
    |> Map.values
    |> Seq.map (fun x -> x.divisor)

let init = lcm (Seq.head divisors) ((Seq.tail >> Seq.head) divisors)

let lowest = divisors |> Seq.skip 2 |> Seq.fold lcm init

let newIterate (all: Map<int,Monkey>, inspections: Map<int, bigint>) (index: int) =
    let m = Map.find index all
    let inspected = m.items |> Array.map m.operation |> Array.map (fun x -> x % bigint lowest)
    let currentMap = updateMonkey index (fun m -> { m with items = [||] }) all

    let updatedMap =
        inspected
        |> Array.fold (fun curr i -> updateMonkey (m.test i) (catch i) curr) currentMap

    let updatedInspections = inspections |> Map.change index (fun x ->
        match x with
        | Some v -> Some (v + bigint (Array.length inspected))
        | None   -> None)

    (updatedMap, updatedInspections)


let _, newResults =
    [1..10000]
    |> List.fold (fun current iter ->
        monkeys
        |> Map.keys
        |> Seq.fold newIterate current) (monkeys, inspections)

printfn "new score: %A" (score newResults)
