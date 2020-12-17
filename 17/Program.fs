open System.IO

type State = Set<int * int * int>

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines("input.txt")
    
    let parseInput (inputLines: string[]) =
        [|
            for y in 0 .. inputLines.Length - 1 do
                for x in 0 .. inputLines.[y].Length - 1 do
                    if inputLines.[y].[x] = '#' then yield (x, y, 0) |]

    let initialState3 = parseInput input
    let initialState4 = initialState3 |> Array.map (fun (x, y, z) -> (x, y, z, 0))
    
    let calculateNextState3 prevState =
        let minX = prevState |> Seq.map (fun (x, _, _) -> x) |> Seq.min
        let maxX = prevState |> Seq.map (fun (x, _, _) -> x) |> Seq.max
        let minY = prevState |> Seq.map (fun (_, y, _) -> y) |> Seq.min
        let maxY = prevState |> Seq.map (fun (_, y, _) -> y) |> Seq.max
        let minZ = prevState |> Seq.map (fun (_, _, z) -> z) |> Seq.min
        let maxZ = prevState |> Seq.map (fun (_, _, z) -> z) |> Seq.max
        
        let prevCubes = prevState |> Set.ofArray   
        let isActive cube = prevCubes.Contains cube
        let isInactive cube = not (isActive cube)
        
        let neighbors (cx, cy, cz) =
            seq {
                for x in cx - 1 .. cx + 1 do
                    for y in cy - 1 .. cy + 1 do
                        for z in cz - 1 .. cz + 1 do
                            if (x, y, z) <> (cx, cy, cz) then yield (x, y, z) }
        
        let numberOfActiveNeighbors cube =
            neighbors cube
            |> Seq.filter isActive
            |> Seq.length
        
        [|
            for x in minX - 1 .. maxX + 1 do
                for y in minY - 1 .. maxY + 1 do
                    for z in minZ - 1 .. maxZ + 1 do
                        let n = numberOfActiveNeighbors (x, y, z)
                        if (isActive (x, y, z) && (n = 2 || n = 3)) || (isInactive (x, y, z) && n = 3)
                        then yield (x, y, z) |]
        
    let calculateNextState4 prevState =
        let minX = prevState |> Seq.map (fun (x, _, _, _) -> x) |> Seq.min
        let maxX = prevState |> Seq.map (fun (x, _, _, _) -> x) |> Seq.max
        let minY = prevState |> Seq.map (fun (_, y, _, _) -> y) |> Seq.min
        let maxY = prevState |> Seq.map (fun (_, y, _, _) -> y) |> Seq.max
        let minZ = prevState |> Seq.map (fun (_, _, z, _) -> z) |> Seq.min
        let maxZ = prevState |> Seq.map (fun (_, _, z, _) -> z) |> Seq.max
        let minW = prevState |> Seq.map (fun (_, _, _, w) -> w) |> Seq.min
        let maxW = prevState |> Seq.map (fun (_, _, _, w) -> w) |> Seq.max
        
        let prevCubes = prevState |> Set.ofArray   
        let isActive cube = prevCubes.Contains cube
        let isInactive cube = not (isActive cube)
        
        let neighbors (cx, cy, cz, cw) =
            seq {
                for x in cx - 1 .. cx + 1 do
                    for y in cy - 1 .. cy + 1 do
                        for z in cz - 1 .. cz + 1 do
                            for w in cw - 1 .. cw + 1 do
                                if (x, y, z, w) <> (cx, cy, cz, cw) then yield (x, y, z, w) }
        
        let numberOfActiveNeighbors cube =
            neighbors cube
            |> Seq.filter isActive
            |> Seq.length
        
        [|
            for x in minX - 1 .. maxX + 1 do
                for y in minY - 1 .. maxY + 1 do
                    for z in minZ - 1 .. maxZ + 1 do
                        for w in minW - 1 .. maxW + 1 do
                            let n = numberOfActiveNeighbors (x, y, z, w)
                            if (isActive (x, y, z, w) && (n = 2 || n = 3)) || (isInactive (x, y, z, w) && n = 3)
                            then yield (x, y, z, w) |]

    {1..6} 
    |> Seq.fold (fun state _ -> calculateNextState3 state) initialState3
    |> Array.length
    |> printfn "Answer 1: %i"
    
    {1..6} 
    |> Seq.fold (fun state _ -> calculateNextState4 state) initialState4
    |> Array.length
    |> printfn "Answer 2: %i"
    
    0 // return an integer exit code
