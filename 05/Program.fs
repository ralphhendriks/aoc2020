open System.IO

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")

    let decodeSeatId boardingPass =
        let charToBit zeroChar oneChar char =
            match char with
            | c when c = zeroChar -> 0
            | c when c = oneChar -> 1
            | _ -> failwith "Unknown character"

        let row =
            boardingPass
            |> Seq.take 7
            |> Seq.fold (fun acc c -> (acc <<< 1) ||| (charToBit 'F' 'B' c)) 0

        let col =
            boardingPass
            |> Seq.skip 7
            |> Seq.take 3
            |> Seq.fold (fun acc c -> (acc <<< 1) ||| (charToBit 'L' 'R' c)) 0

        row * 8 + col

    let registeredSeats = input |> Array.map decodeSeatId
    let maxSeat = registeredSeats |> Array.max
    printfn "Answer 1: %i" maxSeat

    let allSeats =
        seq {
            for row in 0 .. 127 do
                for col in 0 .. 7 -> row * 8 + col
        }

    let minSeat = registeredSeats |> Array.min
    let vacantSeat =
        allSeats
        |> Seq.filter (fun x -> x >= minSeat)
        |> Seq.filter (fun x -> x <= maxSeat)
        |> Seq.filter (fun x -> not (Array.contains x registeredSeats))
        |> Seq.head

    printfn "Answer 2: %i" vacantSeat

    0 // return an integer exit code
