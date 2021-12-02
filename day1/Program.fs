// Learn more about F# at http://fsharp.org

open System.IO

let readLines (filePath:string) =  [
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield int(sr.ReadLine())
]

(*
original count increments function before using a fold

let rec helper input prev count =
    match input with
    | [] -> count
    | x::xs ->
        match prev with
        | None -> helper xs (Some x) count
        | Some prev -> 
            if prev < x then
                helper xs (Some x) (count + 1)
            else
                helper xs (Some x) count
helper depthList None 0
*)

let countInc depthList =
    let folder (prev : int option, count : int) (x : int) =
        match prev with
        | None -> Some x, count
        | Some prev ->
            if prev < x then
                Some x, (count + 1)
            else
                Some x, count
    let (_, count) = List.fold folder (None, 0) depthList
    count

exception WindowSizeException of string

let countIncWindow depthList =
    let folder (window : int list, count : int) (x : int) =
        match window with
        | [] -> [x], count
        | [first] -> [first; x], count
        | [first;second] -> [first; second; x], count
        | [first;second;third] ->
            let prev = first + second + third
            let cur = second + third + x
            if prev < cur then
                [second; third; x], count + 1
            else
                [second; third; x], count
        | _ -> raise (WindowSizeException "Window should contain 3 or fewer elements")

    let (_, count) = List.fold folder ([], 0) depthList
    count

[<EntryPoint>]
let main argv =
    let depthList = readLines argv.[0]
    let count = countInc depthList
    printfn "incrementations = %d" count
    let count = countIncWindow depthList
    printfn "incrementations = %d" count
    0
