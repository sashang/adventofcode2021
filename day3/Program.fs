// Learn more about F# at http://fsharp.org

open System
open System.IO

let readLines =  [
    use sr = new StreamReader(Console.OpenStandardInput())
    while not sr.EndOfStream do
        yield sr.ReadLine()
]

let countOnes (state : seq<int>) (binary : string) =
    let zipped = Seq.zip state binary
    seq {
        for (count, x) in zipped do
            yield if x = '1' then count + 1 else count
    }

let calcGammaAndEpsilon input =
    let state = (11, 0 ,0)
    let _, g, e =
        Seq.fold (fun (exponent, gamma, epsilon) x ->
            if x < 500 then
                (exponent-1, gamma, epsilon + pown 2 exponent)
            else
                (exponent - 1, gamma + pown 2 exponent, epsilon))
            state
            input
    g, e

[<EntryPoint>]
let main _ =
    let count = readLines |> List.head |> String.length
    let initState = seq { for _ in [0..count] do yield 0 }
    let g,e =
        readLines
        |> List.fold countOnes initState
        |> calcGammaAndEpsilon
    printfn "gamma = %d  epsilon = %d" g e
    0 // return an integer exit code
