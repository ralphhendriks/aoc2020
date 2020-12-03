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
            | Regex @"(\d+)-(\d+) ([a-z]): ([a-z]+)" [ pos1; pos2; char; password ] ->
                Some(int pos1, int pos2, char.[0], password)
            | _ -> None)
        |> Array.choose id
    
    entries
    |> Seq.filter (fun (min, max, char, password) ->
        let actual =
            password
            |> Seq.where (fun c -> c = char)
            |> Seq.length
        actual >= min && actual <= max)
    |> Seq.length
    |> printfn "Answer 1: %i"
    
    entries
    |> Seq.filter (fun (pos1, pos2, char, password) ->
        let c1 = password.[pos1 - 1]
        let c2 = password.[pos2 - 1]
        (c1 = char && c2 <> char) || (c1 <> char && c2 = char))
    |> Seq.length
    |> printfn "Answer 2: %i"

    0 // return an integer exit code
