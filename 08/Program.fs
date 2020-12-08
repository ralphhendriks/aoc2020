open System.IO

type ComputerState =
    | LoopDetected of instr: int * acc: int
    | Terminated of instr: int * acc: int

[<EntryPoint>]
let main _ =
    let program =
        File.ReadAllLines("input.txt")
        |> Array.map (fun l ->
            match l.Split(" ") with
            | [| instr; arg1 |] -> (instr, int arg1)
            | _ -> failwith "syntax error" )
        
    let rec runGameConsole (program: (string * int )[]) visited (ptr, acc) =
        if ptr >= program.Length then Terminated(ptr, acc)
        elif Set.contains ptr visited then LoopDetected(ptr, acc)
        else
            match program.[ptr] with
            | ("acc", arg1) -> runGameConsole program (Set.add ptr visited) (ptr + 1, acc + arg1)
            | ("jmp", arg1) -> runGameConsole program (Set.add ptr visited) (ptr + arg1, acc)
            | ("nop", _) -> runGameConsole program (Set.add ptr visited) (ptr + 1, acc)
            | _ -> failwith "invalid instruction"
    
    match runGameConsole program Set.empty (0, 0) with
    | LoopDetected(_, acc) -> printfn "Answer 1: %i" acc
    | _ -> failwith "No infinite loop found"
    
    seq { 0 .. program.Length - 1 }
    |> Seq.filter (fun i -> (fst program.[i]) = "nop" || (fst program.[i]) = "jmp")
    |> Seq.pick (fun i ->
        let programCopy = Array.copy program
        programCopy.[i] <- ((if (fst programCopy.[i] = "nop") then "jmp" else "nop"), snd programCopy.[i])
        match runGameConsole programCopy Set.empty (0, 0) with
        | Terminated(_, acc) -> Some acc
        | _ -> None)
    |> printfn "Answer 2: %i"
    
    0 // return an integer exit code
