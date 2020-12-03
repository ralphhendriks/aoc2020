open System
open System.IO
open System.Text.RegularExpressions

let (|Int|_|) (str:string) =
    match Int32.TryParse str with
    | true,int -> Some int
    | _ -> None

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt")
    
    let passports =
        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.map (fun l ->
            l.Split([| " "; Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun r ->
                let x = r.Split(':')
                (x.[0], x.[1]))
            |> Map.ofArray)

    let isValidPolicy1 (passport: Map<string, string>) =
        [| "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" |]
        |> Array.forall (fun x -> passport.ContainsKey x)
            
    passports
    |> Seq.filter isValidPolicy1
    |> Seq.length
    |> printfn "Answer 1: %i"

    let isValidPolicy2 (passport: Map<string, string>) =
        let byr =
            match passport.TryFind "byr" with
            | Some (Int y) when 1920 <= y && y <= 2002 -> true
            | _ -> false
        let iyr =
            match passport.TryFind "iyr" with
            | Some (Int y) when 2010 <= y && y <= 2020 -> true
            | _ -> false
        let eyr =
            match passport.TryFind "eyr" with
            | Some (Int y) when 2020 <= y && y <= 2030 -> true
            | _ -> false
        let hgt =
            match passport.TryFind "hgt" with
            | Some (Regex "(\d\d\d)cm" [cm]) -> 150 <= (int cm) && (int cm) <= 193
            | Some (Regex "(\d\d)in" [inch]) -> 59 <= (int inch) && (int inch) <= 76
            | _ -> false
        let hcl =
            match passport.TryFind "hcl" with
            | Some (Regex "#[0-9a-f]{6}" _) -> true
            | _ -> false
        let ecl =
            match passport.TryFind "ecl" with
            | Some "amb" | Some "blu" | Some "brn" | Some "gry" | Some "grn" | Some "hzl" | Some "oth" -> true
            | _ -> false
        let pid =
            match passport.TryFind "pid" with
            | Some (Regex "^[0-9]{9}$" _) -> true
            | _ -> false
        byr && iyr && eyr && hgt && hcl && ecl && pid
    
    passports
    |> Seq.filter isValidPolicy2
    |> Seq.length
    |> printfn "Answer 2: %i"
        
    0 // return an integer exit code
