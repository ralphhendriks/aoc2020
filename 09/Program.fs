open System.IO

[<EntryPoint>]
let main _ =
    let numbers = File.ReadAllLines("input.txt") |> Array.map int64
    let preambleLength = 25

    let idx =
        seq { preambleLength .. (numbers.Length - 1) }
        |> Seq.find (fun i ->
            not
                (seq { i - preambleLength .. i - 2 }
                 |> Seq.exists (fun pi -> Array.contains (numbers.[i] - numbers.[pi]) numbers.[pi + 1..i - 1])))
    printfn "Answer 1: %i" numbers.[idx]

    let allContiguousSets (n: int64[])  =
        seq { for l in 2 .. n.Length do yield! Array.windowed l n }

    Seq.concat [allContiguousSets numbers.[0..idx - 1]; allContiguousSets numbers.[idx + 1..numbers.Length - 1]]
    |> Seq.find (fun l -> Array.sum l = numbers.[idx])
    |> fun l -> (Array.min l) + (Array.max l)
    |> printfn "Answer 2: %i"

    0 // return an integer exit code
