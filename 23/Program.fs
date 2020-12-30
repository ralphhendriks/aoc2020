type Game = int * Map<int, int> * int

[<EntryPoint>]
let main _ =
//    let input = "389125467" // test
    let input = "318946572" // my input

    let parseInput (input: string) : Game =
        let cups = input |> Seq.toArray |> Array.map (fun c -> int c - 48)
        (cups.[0], cups |> Array.pairwise |> Map.ofArray |> Map.add cups.[cups.Length - 1] cups.[0], cups.Length)

    let playRound ((currentCup, cups, numberOfCups): Game) : Game =
        let remove1 = cups.[currentCup]
        let remove2 = cups.[remove1]
        let remove3 = cups.[remove2]
        let nextCup = cups.[remove3]

        let destinationCup =
            seq {
                for i in currentCup + numberOfCups - 1 .. -1 .. currentCup + 1 ->
                    if i = numberOfCups then i else i % numberOfCups
            }
            |> Seq.find (fun c -> c <> remove1 && c <> remove2 && c <> remove3)
        let cupAfterDestinationCup = cups.[destinationCup]

        let nextCups =
            cups
            |> Map.add destinationCup remove1
            |> Map.add remove3 cupAfterDestinationCup
            |> Map.add currentCup nextCup

        (nextCup, nextCups, numberOfCups)
    
    let score1 ((_, cups, _): Game) =
        let rec loop collected =
            match collected with
            | x :: _ when cups.[x] = 1 -> collected
            | x :: _ -> loop (cups.[x] :: collected)
            | _ -> failwith "list may not be empty"
        loop [cups.[1]] |> List.rev |> List.map string |> List.reduce (+)
    
    let playRounds rounds game = seq {1 .. rounds} |> Seq.fold (fun s _ -> playRound s) game

    input |> parseInput |> playRounds 100 |> score1 |> printfn "Answer 1: %s"

    let augmentGame newNumberOfCups ((currentCup, cups, numberOfCups): Game) : Game =
        if newNumberOfCups <= numberOfCups then (currentCup, cups, numberOfCups)
        else
            let cupBeforeCurrent = cups |> Map.pick (fun k v -> if v = currentCup then Some k else None)
            let newCups =
                [numberOfCups + 1 .. newNumberOfCups]
                |> List.pairwise
                |> Map.ofList
            let modifiedCups =
                Map.fold (fun acc key value -> Map.add key value acc) newCups cups
                |> Map.add cupBeforeCurrent (numberOfCups + 1)
                |> Map.add newNumberOfCups currentCup
            (currentCup, modifiedCups, newNumberOfCups)
    
    let score2 ((_, cups, _): Game) =
        let cup1 = cups.[1]
        int64 cup1 * int64 cups.[cup1]
           
    input |> parseInput |> augmentGame 1_000_000 |> playRounds 10_000_000 |> score2 |> printfn "Answer 2: %i"
    
    0 // return an integer exit code
