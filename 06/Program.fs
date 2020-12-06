open System
open System.IO

[<EntryPoint>]
let main _ =
    let groups =
        File.ReadAllText("input.txt").Split(Environment.NewLine + Environment.NewLine)
        |> Array.map (fun group ->
            group.Split(Environment.NewLine)
            |> Seq.map Set.ofSeq)

    groups
    |> Array.map (Set.unionMany >> Set.count)
    |> Array.sum
    |> printfn "Answer 1: %i"

    groups
    |> Array.map (Set.intersectMany >> Set.count)
    |> Array.sum
    |> printfn "Answer 2: %i"

    0 // return an integer exit code
