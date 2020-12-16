open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText("input.txt")
    let sep1 = "your ticket:"
    let sep2 = "nearby tickets:"
    let index1 = input.IndexOf(sep1)
    let index2 = input.IndexOf(sep2)
    let fieldLines = input.Substring(0, index1).Trim().Split(Environment.NewLine)
    let yourTicketLine = input.Substring(index1 + sep1.Length, index2 - index1 - sep1.Length).Trim()
    let nearbyTicketsLines = input.Substring(index2 + sep2.Length).Trim().Split(Environment.NewLine)
    
    let parseFields (lines: string []) =
        lines
        |> Array.map (fun line ->
            let idx = line.IndexOf(':')
            let name = line.Substring(0, idx).Trim()
            let values =
                line.Substring(idx + 2).Trim().Split(" or ")
                |> Array.toList
                |> List.collect (fun range ->
                    let parts = range.Split('-')
                    [ int parts.[0] .. int parts.[1] ])
                |> Set.ofList
            (name, values))
        |> Map.ofArray
    
    let parseTicket (line: string) = line.Split(',') |> Array.toList |> List.map int
    
    let parseNearbyTickets (lines: string[]) =
        lines
        |> Array.toList
        |> List.map parseTicket

    let fields = parseFields fieldLines
    let yourTicket = parseTicket yourTicketLine
    let nearbyTickets = parseNearbyTickets nearbyTicketsLines
    
    let knownValues =
        fields
        |> Map.toList    
        |> List.map snd
        |> List.reduce Set.union

    let allInvalidNearbyValues =
        nearbyTickets
        |> List.collect id
        |> List.filter (fun v -> not (knownValues.Contains v))
    
    let answer1 = allInvalidNearbyValues |> List.sum
    
    let allValidTickets =
        yourTicket :: nearbyTickets
        |> List.filter (fun t -> List.forall (fun v -> knownValues.Contains v) t)
    
    let allValues = List.transpose allValidTickets
    
    let fieldCandidates =
        allValues
        |> List.map (fun values ->
            fields
            |> Map.toList
            |> List.filter (fun (_, v) -> List.forall (fun x -> v.Contains x) values)
            |> List.map fst)

    
    let fieldMapping =
        fieldCandidates
        |> List.indexed
        |> List.sortBy (fun (_, v) -> v.Length)
        |> List.fold (fun acc (i, candidates) ->
            let known = List.map snd acc
            let exceptKnown = List.filter (fun c -> not (List.contains c known)) candidates
            if exceptKnown.Length = 1
            then (i, exceptKnown.[0]) :: acc
            else failwith "clever filtering failed") []
    
    let answer2 =
        fieldMapping
        |> List.filter (fun (i, f) -> f.StartsWith "departure")
        |> List.map (fun (i, _) -> int64 (yourTicket.Item i))
        |> List.reduce (*)
    
//    printfn "%A" fieldLines
//    printfn "%A" yourTicketLine
//    printfn "%A" nearbyTicketsLines
//    printfn "%A" fields
//    printfn "%A" knownValues
//    printfn "%A" allInvalidNearbyValues
//    printfn "Answer 1: %i" answer1
    
    printfn "%A" allValidTickets
    printfn "%A" allValues
    printfn "%A" fieldCandidates
    printfn "%A" fieldMapping
    printfn "%A" answer2
    
    0 // return an integer exit code