module naturalselection

    open organism.types

    let toothAndClaw generation size =
        // Returns the `size` fittest members of generation
        let generation = generation |> (fun (Generation g) -> g)
        let rec f acc x s =
             match s with
                |0 -> acc
                |_ -> f ((List.head x) :: acc) (List.tail x) (s - 1)
        
        if (size >= generation.Length) then
            generation
            |> List.sortBy (fun i -> snd i)
            |> Generation
        elif (size <= 0) then
            []
            |> Generation
        else
            f [] generation size
            |> List.sortBy (fun i -> snd i)
            |> Generation

