open System.IO

type Square =
    | Floor
    | Empty
    | Occupied

[<EntryPoint>]
let main _ =
    let charToSeat =
        function
        | '.' -> Floor
        | 'L' -> Empty
        | '#' -> Occupied
        | _ -> failwith "unknown character"

    let waitingArea =
        File.ReadAllLines("input.txt")
        |> Array.map (Seq.map charToSeat)
        |> array2D

    // deze funtie geeft een lijst van alle binnen het grid liggende posities die vanaf positie (r, c) in de
    // richting van de vector (dr, dc) kunnen worden bereikt.
    let allPositionsInDirection grid (r, c) (dr, dc) =
        Seq.initInfinite (fun i -> (r + (i + 1) * dr, c + (i + 1) * dc))
        |> Seq.takeWhile (fun (rr, cc) ->
            0
            <= rr
            && rr < Array2D.length1 grid
            && 0 <= cc
            && cc < Array2D.length2 grid)
        |> Seq.toList

    let allDirections =
        [ (-1, -1)
          (-1, 0)
          (-1, 1)
          (0, -1)
          (0, 1)
          (1, -1)
          (1, 0)
          (1, 1) ]

    let adjacentSeats grid p =
        allDirections
        |> List.choose (fun dir -> List.tryHead (allPositionsInDirection grid p dir))
        |> List.where (fun (r, c) ->
            match grid.[r, c] with
            | Empty
            | Occupied -> true
            | _ -> false)

    let visibleSeats grid p =
        allDirections
        |> List.map (fun d ->
            allPositionsInDirection grid p d
            |> List.tryFind (fun (r, c) ->
                match grid.[r, c] with
                | Empty
                | Occupied -> true
                | Floor -> false))
        |> List.choose id

    let strategy1 (grid: Square [,]) (r, c) =
        match grid.[r, c] with
        | Floor -> None
        | Empty ->
            if adjacentSeats grid (r, c)
               |> List.forall (fun (rr, cc) -> grid.[rr, cc] = Empty) then
                Some(r, c, Occupied)
            else
                None
        | Occupied ->
            if adjacentSeats grid (r, c)
               |> List.where (fun (rr, cc) -> grid.[rr, cc] = Occupied)
               |> List.length
               >= 4 then
                Some(r, c, Empty)
            else
                None

    let strategy2 (grid: Square [,]) (r, c) =
        match grid.[r, c] with
        | Floor -> None
        | Empty ->
            if visibleSeats grid (r, c)
               |> List.forall (fun (rr, cc) -> grid.[rr, cc] = Empty) then
                Some(r, c, Occupied)
            else
                None
        | Occupied ->
            if visibleSeats grid (r, c)
               |> List.where (fun (rr, cc) -> grid.[rr, cc] = Occupied)
               |> List.length
               >= 5 then
                Some(r, c, Empty)
            else
                None

    let mutations (grid: Square [,]) strategy =
        [ for r in 0 .. Array2D.length1 grid - 1 do
            for c in 0 .. Array2D.length2 grid - 1 -> (r, c) ]
        |> List.choose (strategy grid)

    let gridToList (grid: Square [,]) =
        [ for r in 0 .. Array2D.length1 grid - 1 do
            for c in 0 .. Array2D.length2 grid - 1 do
                yield grid.[r, c] ]

    let simulate grid strategy =
        let rec loop () =
            match mutations grid strategy with
            | [] -> ()
            | mut ->
                List.iter (fun (r, c, state) -> grid.[r, c] <- state) mut
                loop ()

        loop ()
        gridToList grid
        |> List.where (fun s -> s = Occupied)
        |> List.length

    let grid1 = Array2D.copy waitingArea
    let answer1 = simulate grid1 strategy1
    printfn "Answer 1: %i" answer1

    let grid2 = Array2D.copy waitingArea
    let answer2 = simulate grid2 strategy2
    printfn "Answer 2: %i" answer2

    0 // return an integer exit code
