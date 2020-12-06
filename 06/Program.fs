open System
open System.IO

[<EntryPoint>]
let main _ =
    let groups = File.ReadAllText("input.txt").Split(Environment.NewLine + Environment.NewLine)
    
    groups
    |> Array.map (fun group ->
        group.Replace(Environment.NewLine, "")
        |> Set.ofSeq
        |> Set.count)
    |> Array.sum
    |> printfn "Answer 1: %i"
    
    groups
    |> Array.map (fun group ->
        group.Split(Environment.NewLine)
        |> Seq.map Set.ofSeq
        |> Set.intersectMany
        |> Set.count)
    |> Array.sum
    |> printfn "Answer 2: %A"
        
    0 // return an integer exit code
