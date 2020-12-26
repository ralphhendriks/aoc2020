open System
open System.IO

type Tile = char[,]

type TileTransformation =
    | Normal
    | Normal90CW
    | Normal180CW
    | Normal270CW
    | Flipped
    | Flipped90CW
    | Flipped180CW
    | Flipped270CW

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText("input.txt")

    let parseInput (input: string) : (int * Tile) list =
        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.toList
        |> List.map (fun tile ->
            let lines = tile.Split(Environment.NewLine)
            let tileNumber = lines.[0].TrimEnd(':').Substring(5) |> int
            let tile = array2D lines.[1..]
            (tileNumber, tile))
        
    let turnCW (tile: Tile) =
        let height, width = Array2D.length1 tile, Array2D.length2 tile
        Array2D.init width height (fun row column -> tile.[height - column - 1, row])
        
    let flipLR (tile: Tile) =
        let height, width = Array2D.length1 tile, Array2D.length2 tile
        Array2D.init height width (fun row column -> tile.[row, width - column - 1])
        
    let printTile (tile: Tile) =
        for r = 0 to Array2D.length1 tile - 1 do
            tile.[r,*] |> String.Concat |> printfn "%s"
    
    let edges (tile: Tile) =
        let t = tile.[0, *] |> String.Concat
        let r = tile.[*, Array2D.length1 tile - 1] |> String.Concat
        let b = tile.[Array2D.length2 tile - 1, *] |> String.Concat
        let l = tile.[*, 0] |> String.Concat
        [t; r; b; l]
    
    let findCorners (tiles: (int * Tile) list) =
        tiles
        |> List.map (fun (i, t) ->
            let otherEdges =
                tiles
                |> List.where (fun (ii, _) -> i <> ii)
                |> List.collect (fun (_, tt) ->
                    let ee = edges tt
                    let rr = ee |> List.map (Seq.rev >> String.Concat)
                    ee @ rr)
            let matches =
                t
                |> edges
                |> List.map (fun ee -> List.contains ee otherEdges)
            (i, matches))
        |> List.filter (fun (_, e) ->
            match e with
            | [true; true; false; false]
            | [false; true; true; false]
            | [false; false; true; true]
            | [true; false; false; false] -> true
            | _ -> false)
        |> List.map (fst >> int64)
        |> List.reduce (*)
    
    printfn "Answer 1: %i" (input |> parseInput |> findCorners)
    
    0 // return an integer exit code
