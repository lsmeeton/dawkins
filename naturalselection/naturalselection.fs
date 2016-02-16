module naturalselection

    open organism.types

    let toothAndClaw (Population population) size =
        // Returns the `size` fittest members of generation
        let rec f acc x s =
             match s with
                |0 -> acc
                |_ -> f ((List.head x) :: acc) (List.tail x) (s - 1)
        
        if (size >= population.Length) then
            population
            |> List.filter (fun o -> match o.fitness with 
                                        |NoFitness -> false
                                        |_ -> true)
            |> List.sortBy (fun o -> o.fitness)
            |> Population
        elif (size <= 0) then
            []
            |> Population
        else
            f [] population size
            |> List.filter (fun o -> match o.fitness with 
                                        |NoFitness -> false
                                        |_ -> true)
            |> List.sortBy (fun o -> o.fitness)
            |> Population

