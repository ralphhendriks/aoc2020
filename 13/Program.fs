open System
open System.IO

let (|Int64|_|) str =
    match Int64.TryParse(str: string) with
    | (true, int) -> Some(int)
    | _ -> None

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")
    let earliestDepartureTime = int64 input.[0]

    let buses =
        input.[1].Split(',')
        |> Array.map (fun c ->
            match c with
            | "x" -> None
            | Int64 n -> Some n
            | _ -> failwith "Unknown input element")

    buses
    |> Array.choose id
    |> Array.map (fun n -> n, n - earliestDepartureTime % n)
    |> Array.minBy snd
    |> fun (n, waitingTime) -> n * waitingTime
    |> printfn "Answer 1: %i"

    let lcm x y =
        let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
        x * y / (gcd x y)

    let folder (per1: int64, start1: int64) (b: int64, i: int64) =
        (lcm per1 b,
         Seq.initInfinite (fun t -> per1 * (int64 t) + start1)
         |> Seq.find (fun t -> (t + i) % b = 0L))

    buses
    |> Array.mapi (fun i bus -> Option.bind (fun b -> Some(b, int64 i)) bus)
    |> Array.choose id
    |> Array.sortBy fst
    |> Array.fold folder (1L, 0L)
    |> snd
    |> printfn "Answer 2: %i"

    0 // return an integer exit code
