open System.IO

let rec combinations2 = function
    | [] -> []
    | x :: xs -> (xs |> List.map (fun y -> (x, y))) @ (combinations2 xs)
    
let rec combinations3 = function
    | [] -> []
    | x :: xs -> (combinations2 xs |> List.map(fun (y, z) -> (x, y, z))) @ (combinations3 xs)

[<EntryPoint>]
let main _ =
    let expenses =
        File.ReadAllLines("input.txt")
        |> Array.map int
        |> Array.toList
    
    let answer1 =
        expenses
        |> combinations2
        |> List.find (fun (x, y) -> x + y = 2020)
        |> fun (x, y) -> x * y
    printfn "Answer 1: %i" answer1
    
    let answer2 =
        expenses
        |> combinations3
        |> List.find (fun (x, y, z) -> x + y + z = 2020)
        |> fun (x, y, z) -> x * y * z
    printfn "Anwer 2: %i" answer2

    0 // return an integer exit code
