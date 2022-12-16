open System.IO

let lines = (File.ReadAllText "simple.txt").Split "\n"

let coordinates = lines |> Array.map (fun x ->
    x.Split " -> " |> Array.map (fun xx ->
        (int ((xx.Split ",")[0]), int ((xx.Split ",")[1]))))

let flatCoords = coordinates |> Array.collect id
let minX = flatCoords |> Array.minBy fst |> fst
let maxX = flatCoords |> Array.maxBy fst |> fst
let minY = 0
let maxY = flatCoords |> Array.maxBy snd |> snd

let source = 500, 0
let a = Array2D.createBased (minX-2) minY (5+maxX-minX) (3+maxY-minY) '.'

let printMap (map: char[,]) =
    [map.GetLowerBound 1 .. map.GetUpperBound 1]
    |> List.iter (fun y -> map[*,y] |> System.String |> printfn "%A")

let paintWith (coords: (int*int) list) (c: char) (map: char[,]) =
    coords |> List.map (fun (x, y) -> map[x, y] <- c)

let walls =
    coordinates
    |> Array.map Array.pairwise
    |> Array.collect (Array.collect (fun ((x1, y1), (x2, y2)) ->
        let startX, stopX = min x1 x2, max x1 x2
        let startY, stopY = min y1 y2, max y1 y2
        Array.allPairs [|startX .. stopX|] [| startY .. stopY |]))
    |> List.ofArray

let fall (grain: int*int) (map: char[,]) =
    match grain with
    | x,y when map[x,   y+1] = '.' -> (x,   y+1)
    | x,y when map[x-1, y+1] = '.' -> (x-1, y+1)
    | x,y when map[x+1, y+1] = '.' -> (x+1, y+1)
    | x,y                          -> (x,   y)

let rec tick (grain: int*int) (sand: (int*int) list) (map: char[,]) =
    match fall grain map with
    | g when snd g = maxY -> sand
    | g when g = grain    ->
        map[fst g, snd g] <- 'o'
        tick source (g :: sand) map
    | g                   -> tick g sand map


paintWith [source] '+' a
paintWith walls '#' a

let sand = tick source [] a
paintWith sand 'o' a

printfn "%A\n" (List.length sand)
printMap a


let height = 1+maxY-minY
let bottom = height + 1
let mutable b = Array2D.createBased (500-height-5) minY (10+2*height) (5+maxY-minY) '.'

let newFall (grain: int*int) (map: byref<char[,]>) =
    match grain with
    | x,y when map[x,   y+1] = '.' -> (x,   y+1)
    | x,y when map[x-1, y+1] = '.' -> (x-1, y+1)
    | x,y when map[x+1, y+1] = '.' -> (x+1, y+1)
    | x,y                          -> (x,   y)

let rec newTick (grain: int*int) (sand: (int*int) list) (map: byref<char[,]>) =
    match newFall grain &map with
    | g when g = grain    ->
        map[fst g, snd g] <- 'o'
        newTick source (g :: sand) &map
    | g when g = source -> sand
    | g -> newTick g sand &map

paintWith [source] '+' b
paintWith walls '#' b
paintWith (List.allPairs [b.GetLowerBound 0 .. b.GetUpperBound 0] [bottom]) '#' b

let mutable newSand = []
let mutable grain = newFall source &b
let mutable prevGrain = source

while grain <> source do
    match grain with
    | g when g = prevGrain    ->
        b[fst g, snd g] <- 'o'
        newSand <- g::newSand
        prevGrain <- grain
        grain <- newFall source &b
    | g when g = source -> ()
    | g ->
        prevGrain <- g
        grain <- newFall g &b

paintWith newSand 'o' b

printMap b
printfn "%A\n" (List.length newSand + 1)
