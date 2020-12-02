open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<EntryPoint>]
let main _ =
    let entries =
        File.ReadAllLines("input.txt")
        |> Array.map (fun a ->
            match a with
            | Regex @"(\d+)-(\d+) ([a-z]): ([a-z]+)" [ min; max; char; password ] ->
                Some(int min, int max, char.[0], password)
            | _ -> None)
        |> Array.choose id
    
    let answer1 =
        entries
        |> Seq.filter (fun (min, max, char, password) ->
            let actual =
                password
                |> Seq.where (fun c -> c = char)
                |> Seq.length
            actual >= min && actual <= max)
        |> Seq.length

    printfn "Answer 1: %i" answer1
    
    let answer2 =
        entries
        |> Seq.filter (fun (min, max, char, password) ->
            match (Seq.tryItem (min - 1) password, Seq.tryItem (max - 1) password) with
            | (Some c1, Some c2) -> (c1 = char || c2 = char) && not (c1 = char && c2 = char)
            | _ -> false)
        |> Seq.length

    printfn "Answer 2: %i" answer2

    0 // return an integer exit code
