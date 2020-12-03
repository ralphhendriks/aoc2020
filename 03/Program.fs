open System.IO

type Square =
    | OpenSpace
    | Tree

[<EntryPoint>]
let main _ =
    let map =
        File.ReadAllLines("input.txt")
    
    let find x y =
        match y with
        | yy when yy < map.Length ->
            let row = map.[yy]
            match row.[x % row.Length] with
            | '.' -> Some OpenSpace
            | '#' -> Some Tree
            | _ -> failwith "Unknown character"
        | _ -> None
    
    let rec path (sx, sy) (x, y) =
        match find x y with
        | Some square ->
            square :: path (sx, sy) ((x + sx), (y + sy))
        | None -> []
    
    let countTrees slope =
        path slope (0, 0)
        |> List.filter (fun s -> s = Tree)
        |> List.length
    
    printfn "Answer 1: %i" (countTrees (3, 1))  

    [| (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) |]
    |> Seq.map countTrees
    |> Seq.map int64
    |> Seq.reduce (fun x y -> x * y)
    |> printfn "Anwer 2: %i"
    
    0 // return an integer exit code
