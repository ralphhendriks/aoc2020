open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type instruction =
    | Mask of uint64 * uint64 * uint64
    | Mem of uint64 * uint64

[<EntryPoint>]
let main _ =
    let parseBitmask (s: string) =
        Mask(
            Convert.ToUInt64(s.PadLeft(64, 'X').Replace('X', '1'), 2),
            Convert.ToUInt64(s.PadLeft(64, 'X').Replace('X', '0'), 2),
            Convert.ToUInt64(s.Replace('1', '0').Replace('X', '1'), 2))
    
    let program =
        File.ReadAllLines("input.txt")
        |> Seq.map (fun l ->
            match l with
            | Regex "^mask = ([X10]{36})$" [bitmask] -> parseBitmask bitmask
            | Regex "^mem\[(\d+)\] = (\d+)$" [address; value] -> Mem(uint64 address, uint64 value)
            | _ -> failwith "unrecognized instruction")    
        
    let applyBitmask (z, o) n = (n ||| o) &&& z
    
    program
    |> Seq.fold (fun (mem, mask) instruction ->
        match instruction with
        | Mem(address, value) -> (Map.add address (applyBitmask mask value) mem, mask)
        | Mask(z, o, _) -> (mem, (z, o))
        ) (Map.empty, (UInt64.MaxValue, UInt64.MinValue))
    |> fst
    |> Map.toSeq
    |> Seq.sumBy snd
    |> printfn "Answer 1: %i"
    
    let generateAddresses (o, x) n =
        [0 .. 35]
        |> List.fold (fun (acc: uint64 list) i ->
            if
                x >>> i &&& 1UL = 1UL
            then // bit is floating
                acc @ List.map (fun z -> z + (1UL <<< i)) acc
            elif
                (o >>> i &&& 1UL = 1UL) || (n >>> i &&& 1UL = 1UL)
            then // bit is one
                List.map (fun z -> z + (1UL <<< i)) acc
            else // bit is zero
                 acc
            ) [0UL]
        
    program
    |> Seq.fold (fun (mem, mask) instruction ->
        match instruction with
        | Mem(address, value) ->
            (generateAddresses mask address |> List.fold (fun m a -> Map.add a value m) mem, mask)
        | Mask(_, o, x) -> (mem, (o, x))
        ) (Map.empty, (UInt64.MaxValue, UInt64.MinValue))
    |> fst
    |> Map.toSeq
    |> Seq.sumBy snd
    |> printfn "Answer 2: %i"
            
    0 // return an integer exit code
