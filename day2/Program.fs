
open System.IO

type Vector =
    | Forward of int
    | Up of int
    | Down of int
    with

    static member init (direction : string) (displacement : string) =
        match direction with
        | "forward" -> Forward (int displacement)
        | "up" -> Up (int displacement)
        | "down" -> Down (int displacement)
        | _ -> invalidArg direction "invalid direction"

type Position = Position of int*int

let readLines (filePath:string) =  [
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        let split = sr.ReadLine().Split(' ')
        yield Vector.init split.[0] split.[1]
]

let execWithAim (position : Position, aim : int) (update : Vector) =
    let (Position(curX, curY)) = position
    match update with
    | Forward x -> Position(curX + x, curY + (aim * x)), aim
    | Up y -> Position(curX, curY), aim - y
    | Down y -> Position(curX, curY), aim + y

[<EntryPoint>]
let main argv =
    let vectors = readLines argv.[0]
    let (Position(x,y), _) = List.fold execWithAim (Position (0,0), 0) vectors
    printfn "x = %d, y = %d, x*y = %d" x y (x*y)
    0 // return an integer exit code
