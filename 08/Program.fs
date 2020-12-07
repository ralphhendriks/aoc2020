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
        
    let rec cycle (program: (string * int )[]) visited (instr, acc) =
        if instr >= program.Length then Terminated(instr, acc)
        elif Set.contains instr visited then LoopDetected(instr, acc)
        else
            match program.[instr] with
            | ("acc", arg1) -> cycle program (Set.add instr visited) (instr + 1, acc + arg1)
            | ("jmp", arg1) -> cycle program (Set.add instr visited) (instr + arg1, acc)
            | ("nop", _) -> cycle program (Set.add instr visited) (instr + 1, acc)
            | _ -> failwith "invalid instruction"
    
    match cycle program Set.empty (0, 0) with
    | LoopDetected(_, acc) -> printfn "Answer 1: %i" acc
    | _ -> failwith "No infinite loop found"
    
    seq { 0 .. program.Length - 1 }
    |> Seq.filter (fun i -> (fst program.[i]) = "nop" || (fst program.[i]) = "jmp")
    |> Seq.map (fun i ->
        let copy = Array.copy program
        copy.[i] <- ((if (fst copy.[i] = "nop") then "jmp" else "nop"), snd copy.[i])
        cycle copy Set.empty (0, 0))
    |> Seq.find (fun result ->
        match result with
        | Terminated _ -> true
        | _ -> false)
    |> fun t ->
        match t with
        | Terminated (_, acc) -> acc
        | _ -> failwith "invalid state"
    |> printfn "Answer 2: %i"
    
    0 // return an integer exit code
