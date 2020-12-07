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
        
    let allBagTypes = luggageRules |> Map.toList |> List.map fst

    let rec bagsContainedBy (qty, color) =
        match Map.find color luggageRules with
        | [] -> []
        | containedBags -> List.collect (fun (q, c) -> (q * qty, c) :: bagsContainedBy (q * qty, c)) containedBags

    allBagTypes
    |> List.filter (fun c -> c <> "shiny gold")
    |> List.map (fun c -> bagsContainedBy (1, c))
    |> List.filter (fun l -> l |> List.exists (fun (_, c) -> c = "shiny gold"))
    |> List.length
    |> printfn "Answer 1: %i"

    bagsContainedBy (1, "shiny gold")
    |> List.map fst
    |> List.sum
    |> printfn "Answer 2: %i"

    0 // return an integer exit code
