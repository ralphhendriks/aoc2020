open System.IO

type Ingredient = string

type Allergene = string

type Food = Set<Ingredient> * Set<Allergene>

[<EntryPoint>]
let main _ =
 
    let parseFood (str: string) : Food =
        let parts = str.TrimEnd(')').Split(" (contains ")
        let ingredients = parts.[0].Split(" ") |> Set.ofArray
        let allergens = parts.[1].Split(", ") |> Set.ofArray
        (ingredients, allergens)
        
    let foods =
        File.ReadAllLines("input.txt")
        |> Array.map parseFood
        |> Array.toList
        
    let allAllergens =
        foods
        |> List.map snd
        |> List.reduce Set.union
        |> Set.toList
    
    printfn "%A" allAllergens
    
    let allergeneCandidates =
        allAllergens
        |> List.map (fun a ->
            let foodCandidates =
                foods
                |> List.filter (fun (_, allergens) -> allergens |> Set.contains a)
                |> List.map fst
                |> List.reduce Set.intersect
            (a, foodCandidates))
        |> Map.ofList
    
    allergeneCandidates |> printfn "%A"
    
    let rec loop (candidates: Map<Allergene, Set<Ingredient>>) =
        let mappedIngredients =
            candidates
            |> Map.filter (fun _ s -> s.Count = 1)
            |> Map.toList
            |> List.map snd
            |> List.reduce Set.union
            
        if mappedIngredients.Count = candidates.Count then candidates
        else
            let filtered =
                candidates
                |> Map.map (fun _ s ->
                    if s.Count = 1 then s
                    else Set.difference s mappedIngredients)
            loop filtered

    let matched =
        loop allergeneCandidates
        |> Map.toList
        |> List.map snd
        |> List.reduce Set.union
    
    let nonMatchedIngredientCount =
        foods
        |> List.map fst
        |> List.collect Set.toList
        |> List.filter (fun x -> not (matched.Contains x))
        |> List.length
    
    printfn "Answer 1: %i" nonMatchedIngredientCount
    
    let cannonicalDangerousIngredients =
        loop allergeneCandidates
        |> Map.toList
        |> List.map (fun (a, s) -> (a, s |> Set.toList |> List.head))
        |> List.sortBy fst
        |> List.map snd
    
    let answer2 =
        cannonicalDangerousIngredients
        |> List.reduce (fun s1 s2 -> s1 + "," + s2)
        
    printfn "Answer 2: %s" answer2
    
    0 // return an integer exit code
