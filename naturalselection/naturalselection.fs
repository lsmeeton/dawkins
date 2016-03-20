module naturalselection

    open organism.types
    open utility

    let toothAndClaw (Population population) size =
        // Returns the `size` fittest members of generation

        let getFitness organism = 
            match organism.fitness with
               | NoFitness -> 10000000.0 // a large float...
               | Fitness f-> f
        
        if (size <= 0) then
            []
            |> Population
        else
            population//f [] population size
            |> List.filter (fun o -> match o.fitness with 
                                        |NoFitness -> false
                                        |_ -> true)
            |> List.sortBy (fun o -> -1.0 * (getFitness o)) // Reverse sort
            |> truncateList size
            |> List.rev // 
            |> Population

