open System.IO

[<EntryPoint>]
let main _ =
    let adapters = File.ReadAllLines("input.txt") |> Array.map int |> Array.sort

    let builtInAdapter = 3 + Array.last adapters

    Array.concat [ [|0|]; adapters ; [|builtInAdapter|] ]
    |> Array.pairwise
    |> Array.map (fun (p, q) -> q - p)
    |> Array.countBy id
    |> Map.ofArray
    |> fun m -> (Map.find 1 m) * (Map.find 3 m)
    |> printfn "Answer 1: %i"

    Array.append adapters [|builtInAdapter|]
    |> Array.fold (fun paths adapter ->
            let withAllowedInputJoltage = paths |> Map.filter (fun k _ -> adapter - k < 4)
            withAllowedInputJoltage.Add (adapter, withAllowedInputJoltage |> Map.fold (fun acc _ v -> acc + v) 0L)
        ) (Map.empty.Add(0, 1L))
    |> Map.find builtInAdapter
    |> printfn "Answer 2: %i"

    0 // return an integer exit code
