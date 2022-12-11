open System.IO

let lines = (File.ReadAllText "input.txt").Split "\n"

type FileSystemItem =
    | File of FileData
    | Dir of DirectoryData
and FileData = { name:string; fileSize:int }
and DirectoryData = { name:string; items: Map<string, FileSystemItem> }

let parseData (a: string) =
    let parts = a.Split " "
    match parts[0] with
    | "dir" -> parts[1], Dir { name = parts[1]; items = Map []}
    | _ -> parts[1], File { name = parts[1]; fileSize = int parts[0] }

let lsContents (lines: string list) =
    lines
    |> List.takeWhile (fun x -> not (x.StartsWith "$"))
    |> List.map parseData
    |> Map.ofList


let root = Dir { name = "/"; items = Map [] }

let mutable i = 1
let mutable stack = [root]

while i < Array.length lines do
    let parts = lines[i].Split " "
    match (parts[0], parts[1]) with
    | ("$", "cd") ->
        match parts[2] with
        | ".." ->
            let current = List.head stack
            match current with
            | Dir(c) -> 
                let nextCurrent = stack |> List.tail |> List.head
                match nextCurrent with
                | Dir(n) -> 
                    let newItems = Map.add c.name current n.items
                    let updatedNext = Dir { name = n.name; items = newItems }
                    stack <- updatedNext :: (stack |> List.tail |> List.tail)
                    i <- i + 1
                | _ -> raise (new System.NotImplementedException("fda"))
            | _ -> raise (new System.NotImplementedException("fda"))
        | name -> 
            let current = List.head stack
            match current with
            | Dir(c) -> 
                let targetDir = Map.find name c.items
                stack <- targetDir :: stack
                i <- i + 1
            | _ -> raise (new System.NotImplementedException("fda"))
    | ("$", "ls") -> 
        let current = List.head stack
        match current with
        | Dir(c) -> 
            let newItems = lsContents (lines[i+1..] |> List.ofArray)
            let dir = Dir { name = c.name; items = newItems }
            stack <- dir :: List.tail stack
            i <- i + Map.count newItems + 1
        | _ -> raise (new System.NotImplementedException("asdf"))

    | _ -> raise (new System.NotImplementedException("lol"))

while List.length stack > 1 do
    let current = List.head stack
    match current with
    | Dir(c) -> 
        let nextCurrent = stack |> List.tail |> List.head
        match nextCurrent with
        | Dir(n) -> 
            let newItems = Map.add c.name current n.items
            let updatedNext = Dir { name = n.name; items = newItems }
            stack <- updatedNext :: (stack |> List.tail |> List.tail)
            i <- i + 1
        | _ -> raise (new System.NotImplementedException("fda"))
    | _ -> raise (new System.NotImplementedException("fda"))
    

let tree = List.head stack

let rec sizes (tree: FileSystemItem): list<string * int> =
    match tree with
    | Dir(d) ->
        let (dirs, files) =
            d.items
            |> Map.values 
            |> List.ofSeq
            |> List.partition (function Dir _ -> true | _ -> false)
        let filesSize =
            files
            |> List.map (fun x ->
                match x with
                | File(f) -> f.fileSize
                | _ -> 0)
            |> List.sum
        let dirSizes =
            dirs
            |> List.map (fun x ->
                match x with
                | Dir(_) -> sizes x
                | _ -> raise (new System.NotImplementedException("fda")))
        let directChildren =
            dirSizes
            |> List.map List.head
        let foldersSize =
            directChildren
            |> List.map (fun (_, size) -> size)
            |> List.sum
        (d.name, filesSize + foldersSize) :: List.collect id dirSizes

        
    | _ -> raise (new System.NotImplementedException("fda"))

let allSizes = sizes tree

let selectedSizes =
    allSizes
    |> List.map (fun (_, size) -> size)
    |> List.filter (fun x -> x < 100001)
    |> List.sum

printfn "small sum sizes: %A" selectedSizes

let systemSize = 70000000
let neededSpace = 30000000

let currentStatus = systemSize - (snd (allSizes |> List.head))
let toBeFreed = neededSpace - currentStatus

let smallestSize =
    allSizes
    |> List.map (fun (_, size) -> size)
    |> List.filter (fun x -> x > toBeFreed)
    |> List.min

printfn "current status: %A" currentStatus
printfn "to be freed: %A" toBeFreed
printfn "Smallest fulfilling: %A" smallestSize
