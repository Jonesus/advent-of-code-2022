open System.IO

let start = [|
    "VNFSMPHJ";
    "QDJMLRS";
    "BWSCHDQN";
    "LCSR";
    "BFPTVM";
    "CNQRT";
    "RVG";
    "RLDPSZC";
    "FBPGVJSD";
|]
let status = start |> Array.map (Seq.rev >> Seq.toArray)

let lines = (File.ReadAllText "input.txt").Split "\n"

let getInstructions (x: string) =
    let items = x.Split " "
    let count = int items[1]
    let from = int items[3]
    let target = int items[5]
    (count, from-1, target-1)

let executeInstructions (xs: char[][]) (count: int, from: int, target: int) =
    let splitIndex = Array.length xs[from] - count
    let (stay, move) = Array.splitAt splitIndex xs[from]

    xs[from] <- stay
    xs[target] <- Array.concat [xs[target]; Array.rev move]

do lines
    |> Array.map getInstructions
    |> Array.iter (executeInstructions status)

let result = new string(status |> Array.map Array.last)

printfn "End result: %A" result

let newStatus = start |> Array.map (Seq.rev >> Seq.toArray)

let executeNewInstructions (xs: char[][]) (count: int, from: int, target: int) =
    let splitIndex = Array.length xs[from] - count
    let (stay, move) = Array.splitAt splitIndex xs[from]

    xs[from] <- stay
    xs[target] <- Array.concat [xs[target]; move]

do lines
    |> Array.map getInstructions
    |> Array.iter (executeNewInstructions newStatus)

let newResult = new string(newStatus |> Array.map Array.last)

printfn "New result: %A" newResult
