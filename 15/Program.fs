open System
open System.IO

[<EntryPoint>]
let main _ =
    let startingNumbers =
        File.ReadAllText("input.txt")
        |> fun s -> s.Split(',')
        |> Array.map int

    let numberSpoken turn =
        [ 1 .. turn ]
        |> List.fold (fun (last, indices) n ->
            let next =
                if n <= startingNumbers.Length then startingNumbers.[n - 1]
                elif Map.containsKey last indices then n - indices.[last] - 1
                else 0
            next, Map.add last (n - 1) indices) (0, Map.empty)
        |> fst

    numberSpoken 2020 |> printfn "Answer 1: %i"
    numberSpoken 30000000 |> printfn "Answer 2: %i"

    0 // return an integer exit code
