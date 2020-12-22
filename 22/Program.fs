open System.IO

type Deck = int list

type Winner =
    | Winner1 of Deck
    | Winner2 of Deck

[<EntryPoint>]
let main _ =
    
    let parseInput (lines: string list) : Deck * Deck =
        let rec loop lines player1 player2 readingPlayer2 =
            match lines with
            | [] -> (List.rev player1, List.rev player2)
            | "Player 1:" :: ls -> loop ls player1 player2 false
            | "" :: ls -> loop ls player1 player2 false
            | "Player 2:" :: ls -> loop ls player1 player2 true
            | l :: ls when not readingPlayer2 -> loop ls (int l :: player1) player2 false
            | l :: ls -> loop ls player1 (int l :: player2) true
        loop lines [] [] false
    
    let rec playRegularCombat (player1: Deck, player2: Deck) =
        match (player1, player2) with
        | ([], []) -> failwith "Both decks empty, not possible"
        | (p1, []) -> Winner1 p1
        | ([], p2) -> Winner2 p2
        | (p1h :: p1t, p2h :: p2t) ->
            if p1h > p2h then
                playRegularCombat (p1t @ [p1h; p2h], p2t)
            else
                playRegularCombat (p1t, p2t @ [p2h; p1h])
    
    let playRecursiveCombat (player1: Deck, player2: Deck) : Winner =
        let rec loop (player1: Deck, player2: Deck) prevDecks : Winner =
            if (prevDecks |> Set.contains (player1, player2)) then Winner1 player1
            else
                let theseDecks = prevDecks |> Set.add (player1, player2)
                match (player1, player2) with
                | ([], []) -> failwith "Both decks empty, not possible"
                | (p1, []) -> Winner1 p1
                | ([], p2) -> Winner2 p2
                | (p1h :: p1t, p2h :: p2t) ->
                    if p1h <= p1t.Length && p2h <= p2t.Length
                    then
                        match loop (p1t |> List.take p1h, p2t |> List.take p2h) theseDecks with
                        | Winner1 _ -> loop (p1t @ [p1h; p2h], p2t) theseDecks
                        | Winner2 _ -> loop (p1t, p2t @ [p2h; p1h]) theseDecks
                    else
                        if p1h > p2h then
                            loop (p1t @ [p1h; p2h], p2t) theseDecks
                        else
                            loop (p1t, p2t @ [p2h; p1h]) theseDecks
        loop (player1, player2) Set.empty
            
    let calculateScore (deck: Deck) =
        deck
        |> List.rev
        |> List.indexed
        |> List.map (fun (i, c) -> (i + 1) * c)
        |> List.sum
    
    let input = File.ReadAllLines("input.txt")
    
    input
    |> Array.toList
    |> parseInput
    |> playRegularCombat
    |> (fun w ->
        match w with
        | Winner1 d -> calculateScore d
        | Winner2 d -> calculateScore d)
    |> printfn "Answer 1: %i"

    input
    |> Array.toList
    |> parseInput
    |> playRecursiveCombat
    |> (fun w ->
        match w with
        | Winner1 d -> calculateScore d
        | Winner2 d -> calculateScore d)
    |> printfn "Answer 1: %i"

        
    0 // return an integer exit code
