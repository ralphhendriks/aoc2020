open System.IO

type Rule =
    | ValueRule of char
    | SubRule of int list list

[<EntryPoint>]
let main _ =
        
    let parseRuleDescription (s: string) =
        let parts = s.Split ": "
        let ruleNumber = int parts.[0]
        if parts.[1].StartsWith "\""
        then (ruleNumber, ValueRule(parts.[1].[1]))
        else
            let subRules =
                parts.[1].Split " | "
                |> Array.map (fun x ->
                    x.Split " "
                    |> Array.map int
                    |> Array.toList)
                |> Array.toList
            (ruleNumber, SubRule(subRules))

    let combine (lhs : Result<string list, unit>) (rhs: Result<string list, unit>) : Result<string list, unit> = 
        match lhs, rhs with
        | Ok l, Ok r -> l@r |> List.distinct |> Ok
        | Ok l, Error _ -> Ok l
        | Error _, Ok r -> Ok r
        | _ -> Error ()
    
    let rec evaluateRule (rules: Map<int, Rule>) (ruleId: int) (msg: string) =
        
        let evaluateValueRule (charToMatch: char) (str: string) =
            if str.StartsWith(charToMatch)
            then Ok [str.[1 ..]]
            else Error ()
        
        let rec evaluateChainedRules (ruleIds: int list) (str: string) =
            match ruleIds with
            | [] -> Ok [str]
            | rh :: rt ->
                match evaluateRule rules rh str with
                | Ok rems ->
                    rems
                    |> List.map (fun rem -> evaluateChainedRules rt rem)
                    |> List.reduce combine
                | _ -> Error ()
           
        let evaluateAlternativeRules (ruleIds: int list list) (str: string) =
            ruleIds
            |> List.map (fun a -> evaluateChainedRules a str)
            |> List.reduce combine
        
        match rules.[ruleId] with
        | ValueRule c -> evaluateValueRule c msg
        | SubRule l -> evaluateAlternativeRules l msg

    let applies (rules: Map<int, Rule>) (ruleId: int) (msg: string) =
        match evaluateRule rules ruleId msg with
        | Ok rems when Seq.exists (fun r -> r = "") rems -> true
        | _ -> false
    
    let input = File.ReadAllLines("input.txt")
    let separatorIndex = Array.findIndex (fun l -> l = "") input
    let ruleBook =
        input.[.. separatorIndex - 1]
        |> Array.map parseRuleDescription
        |> Map.ofArray
    let receivedMessages = Array.toList input.[separatorIndex + 1 ..]

    receivedMessages
    |> List.filter (applies ruleBook 0)
    |> List.length
    |> printfn "Answer 1: %i"
    
    let modifiedRuleBook =
        ruleBook
        |> Map.add 8 (SubRule [[42]; [42; 8]])
        |> Map.add 11 (SubRule [[42; 31]; [42; 11; 31]])

    receivedMessages
    |> List.filter (applies modifiedRuleBook 0)
    |> List.length
    |> printfn "Answer 2: %i"

    0 // return an integer exit code
