open System.IO

type Square = Empty | Tree

[<EntryPoint>]
let main _ =
    let map = File.ReadAllLines("input.txt")
    
    let elementAt x y =
        match y with
        | yy when yy < map.Length ->
            let row = map.[yy]
            match row.[x % row.Length] with
            | '.' -> Some Empty
            | '#' -> Some Tree
            | _ -> failwith "Unknown character"
        | _ -> None
    
    let countTrees (sx, sy) =
        let rec loop (x, y) =
            match elementAt x y with
            | None -> 0
            | Some Tree -> 1 + loop ((x + sx), (y + sy))
            | Some Empty -> loop ((x + sx), (y + sy))
        loop (0, 0)
    
    printfn "Answer 1: %i" (countTrees (3, 1))  

    [| (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) |]
    |> Seq.map (countTrees >> int64)
    |> Seq.reduce (*)
    |> printfn "Anwer 2: %i"
    
    0 // return an integer exit code
