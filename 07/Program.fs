open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let luggageRules =
        File.ReadAllLines("input.txt")
        |> Array.map (fun l ->
            let b = l.Split(" bags contain ")
            let m = Regex.Matches(b.[1], "(\d+) (\w+ \w+) bags?")
            if m.Count = 0
            then (b.[0], [])
            else (b.[0], [ for i in 0 .. m.Count - 1 -> (int (m.Item i).Groups.[1].Value, (m.Item i).Groups.[2].Value) ]))
        |> Map.ofArray

    let rec bagsContainedBy (qty, color) =
        match luggageRules.TryFind color with
        | Some [] -> [(qty, color)]
        | Some containedBags ->
            let a =
                containedBags
                |> List.map (fun (q, c) ->  bagsContainedBy (q * qty, c))
                |> List.concat
            (qty, color) :: a
        | None -> failwith "no luggage rule specified for color"
        
    seq { for KeyValue(key, _) in luggageRules do yield key }     
    |> Seq.filter (fun c -> c <> "shiny gold")
    |> Seq.map (fun c -> bagsContainedBy (1, c))
    |> Seq.filter (fun l -> l |> List.exists (fun (_, c) -> c = "shiny gold"))
    |> Seq.length
    |> printfn "Answer 1: %i"
    
    bagsContainedBy (1, "shiny gold")
    |> Seq.map fst
    |> Seq.sum
    |> fun i -> i - 1 // the shiny gold bag itself does not count
    |> printfn "Answer 2: %i"
    
    0 // return an integer exit code
