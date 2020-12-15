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
        |> List.fold (fun (history: int list, indices) n ->
            let next =
                if n <= startingNumbers.Length then startingNumbers.[n - 1]
                elif Map.containsKey history.Head indices then n - indices.[history.Head] - 1
                else 0
            next :: history,
            match history with
            | [] -> indices
            | x :: _ -> Map.add x (n - 1) indices) ([], Map.empty)
        |> fst
        |> List.head

    numberSpoken 2020 |> printfn "Answer 1: %i"
    numberSpoken 30000000 |> printfn "Answer 2: %i"

    0 // return an integer exit code
