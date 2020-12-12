open System.IO

type Action = | North| South| East| West| Left| Right| Forward

type Direction = | N | S | E | W

[<EntryPoint>]
let main _ =
    let charToAction =
        function
        | 'N' -> North
        | 'S' -> South
        | 'E' -> East
        | 'W' -> West
        | 'L' -> Left
        | 'R' -> Right
        | 'F' -> Forward
        | _ -> failwith "Unknown character"

    let navigationInstructions =
        File.ReadAllLines("input.txt")
        |> Array.map (fun l -> charToAction l.[0], int (l.Substring 1))
        |> List.ofArray

    let turnRight = function | N -> E | E -> S | S -> W | W -> N
    let turnLeft = function | N -> W | W -> S | S -> E | E -> N

    let navigate (x, y, d) (action, v) =
        match (action, d) with
        | (North, _) | (Forward, N) -> (x, y + v, d)
        | (South, _) | (Forward, S) -> (x, y - v, d)
        | (East, _) | (Forward, E) -> (x + v, y, d)
        | (West, _) | (Forward, W) -> (x - v, y, d)
        | (Left, _) when v = 90 -> (x, y , d |> turnLeft)
        | (Left, _) when v = 180 -> (x, y , d |> turnLeft |> turnLeft)
        | (Left, _) when v = 270 -> (x, y , d |> turnLeft |> turnLeft |> turnLeft)
        | (Right, _) when v = 90 -> (x, y , d |> turnRight)
        | (Right, _) when v = 180 -> (x, y , d |> turnRight |> turnRight)
        | (Right, _) when v = 270 -> (x, y , d |> turnRight |> turnRight |> turnRight)
        | (Left, _) | (Right, _) -> failwith "Unknown turn"
  
    let rotateCW (x, y, wx, wy) = (x, y, wy, -wx)
    let rotateCCW (x, y, wx, wy) = (x, y, -wy, wx)

    let navigateWithNewInstructions (x, y, wx, wy) (action, v) =
        match action with
        | North -> (x, y, wx, wy + v)
        | South -> (x, y, wx, wy - v)
        | East -> (x, y, wx + v, wy)
        | West -> (x, y, wx - v, wy)
        | Left when v = 90 -> (x, y, wx, wy) |> rotateCCW
        | Left when v = 180 -> (x, y, wx, wy) |> rotateCCW |> rotateCCW
        | Left when v = 270 -> (x, y, wx, wy) |> rotateCCW |> rotateCCW |> rotateCCW
        | Right when v = 90 -> (x, y, wx, wy) |> rotateCW
        | Right when v = 180 -> (x, y, wx, wy) |> rotateCW |> rotateCW
        | Right when v = 270 -> (x, y, wx, wy) |> rotateCW |> rotateCW |> rotateCW
        | Forward -> (x + v * wx, y + v * wy, wx, wy)
        | Right | Left -> failwith "unknown direction"
    
    navigationInstructions
    |> List.fold navigate (0, 0, E)
    |> fun (x, y, _) -> abs x + abs y
    |> printfn "Answer 1: %i"

    navigationInstructions
    |> List.fold navigateWithNewInstructions (0, 0, 10, 1)
    |> fun (x, y, _, _) -> abs x + abs y
    |> printfn "Answer 2: %i"
    0 // return an integer exit code
