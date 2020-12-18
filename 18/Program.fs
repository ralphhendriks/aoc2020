open System
open System.IO

type Operation =
    | Plus
    | Times

type Token =
    | Number of int64
    | Operator of Operation
    | LeftParenthesis
    | RightParenthesis

[<EntryPoint>]
let main _ =
    let tryParseInt64 intStr = 
        try
            let i = Int64.Parse intStr
            Some i
        with _ -> None
        
    let tokenize (s: string) =
        s.Replace(" ", "") // remove all spaces
        |> Seq.map (fun c ->
            match tryParseInt64 (string c) with
            | Some i -> Number i
            | None when c = '+' -> Operator Plus
            | None when c = '*' -> Operator Times
            | None when c = '(' -> LeftParenthesis
            | None when c = ')' -> RightParenthesis
            | None -> failwith "Unrecognized char")
        |> Seq.toList
    
    // Edsger Dijkstra's Shunting-yard algorithm
    // See for pseudocode: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    let shunting_yard_LR (input: Token list) =
        let rec loop (tokens: Token list) (output: Token list) (operatorStack: Token List) =
            match tokens with
            | [] ->
                match operatorStack with
                | [] -> output
                | oh :: ot -> loop [] (oh :: output) ot
            | Number i :: tt -> loop tt (Number i :: output) operatorStack
            | Operator t :: tt ->
                match operatorStack with
                | oh :: ot when oh <> LeftParenthesis -> loop tokens (oh :: output) ot // push
                | _ -> loop tt output (Operator t :: operatorStack) // pop
            | LeftParenthesis :: tt -> loop tt output (LeftParenthesis :: operatorStack)
            | RightParenthesis :: tt ->
                match operatorStack with
                | LeftParenthesis :: ot -> loop tt output ot
                | oh :: ot -> loop tokens (oh :: output) ot
                | [] -> failwith "unbalanced parentheses"
        loop input [] [] |> List.rev

    let shunting_yard_modified_precedence (input: Token list) =
        let rec loop (tokens: Token list) (output: Token list) (operatorStack: Token List) =
            match tokens with
            | [] ->
                match operatorStack with
                | [] -> output
                | oh :: ot -> loop [] (oh :: output) ot
            | Number i :: tt -> loop tt (Number i :: output) operatorStack
            | Operator t :: tt ->
                match operatorStack with
                | oh :: ot when oh = Operator Plus -> loop tokens (oh :: output) ot // pop
                | _ -> loop tt output (Operator t :: operatorStack) // push
            | LeftParenthesis :: tt -> loop tt output (LeftParenthesis :: operatorStack)
            | RightParenthesis :: tt ->
                match operatorStack with
                | LeftParenthesis :: ot -> loop tt output ot
                | oh :: ot -> loop tokens (oh :: output) ot
                | [] -> failwith "unbalanced parentheses"
        loop input [] [] |> List.rev
    
    let evaluate (input: Token list) =
        let rec loop (input: Token list) (stack: int64 list) =
            match input with
            | [] -> List.head stack
            | x :: xs ->
                match x with
                | Number i -> loop xs (i :: stack)
                | Operator Plus ->
                    match stack with
                    | s1 :: s2 :: ss -> loop xs (s1 + s2 :: ss)
                    | _ -> failwith "invalid state"
                | Operator Times ->
                    match stack with
                    | s1 :: s2 :: ss -> loop xs (s1 * s2 :: ss)
                    | _ -> failwith "invalid state"
                | _ -> failwith "unexpected token"
        loop input []
    
    File.ReadAllLines("input.txt")
    |> Array.map tokenize
    |> Array.map shunting_yard_LR
    |> Array.map evaluate
    |> Array.sum
    |> printfn "Answer 1: %i"

    File.ReadAllLines("input.txt")
    |> Array.map tokenize
    |> Array.map shunting_yard_modified_precedence
    |> Array.map evaluate
    |> Array.sum
    |> printfn "Answer 1: %i"

//    let test = "5 + 6 * 3"
//    let test = "5 + (6 * 3)"
//    let test = "2 * 3 + (4 * 5)"
//    let test = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
//    let test = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

//    let x = tokenize test
//    printfn "%A" x
//    let y = shunting_yard_modified_precedence x
//    printfn "%A" y
//    let z = evaluate y
//    printfn "%A" z

    0 // return an integer exit code
