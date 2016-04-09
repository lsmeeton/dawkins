module mutate

    open utility
    open organism.types

    let floatShiftMutation phenomeFromGenome costFunction (perturbation : float []) organism =
        // Perturbe the genome of organism by perturbation
        let oldGenome = organism.genome
                        |> fun (FloatGenome g) -> g

        let fitnessFromGenome = phenomeFromGenome >> costFunction

        if oldGenome.Length = perturbation.Length then
            let newGenome = [for o, p in Array.zip oldGenome perturbation do yield o + p]
                            |> List.toArray
                            |> FloatGenome
            {genome = newGenome; phenome = phenomeFromGenome newGenome; fitness = fitnessFromGenome newGenome} 
        else // do nothing
            organism

    let floatStretchMutation phenomeFromGenome costFunction (perturbation : float []) organism =
        // Perturbe the genome of organism by perturbation
        let oldGenome = organism.genome
                        |> fun (FloatGenome g) -> g

        let fitnessFromGenome = phenomeFromGenome >> costFunction

        if oldGenome.Length = perturbation.Length then
            let newGenome = [for o, p in Array.zip oldGenome perturbation do yield o * p]
                            |> List.toArray
                            |> FloatGenome
            {genome = newGenome; phenome = phenomeFromGenome newGenome; fitness = fitnessFromGenome newGenome} 
        else // do nothing
            organism

    let genericMutationOperation mutationProbabilitySource mutationSource = 
        // A generic perturbation template
        let mutations = 
            mutationSource
            |> Seq.head

        let mutationProbabilities = 
            mutationProbabilitySource
            |> Seq.head
            |> truncateList (List.length mutations)
            |> padList false (List.length mutations)

        let wrapMutations mutation mutationProbability = 
            match mutationProbability with
            |true -> Some mutation
            |false -> None

        [for mutation, mutationProbability in 
        List.zip mutations mutationProbabilities
        do yield wrapMutations mutation mutationProbability]
        

    let genericMutateAndReplaceStrategy mutationOperationsSource (Population organisms) = 
            // A generic mutation strategy template

            let mutationOperations  = 
                mutationOperationsSource 
                |> Seq.head
                |> truncateList (List.length organisms) 
                // Handle mutation lists *longer* than organisms
                |> padList None (List.length organisms) 
                // Handle mutation lists *shorter* than organisms by padding with None's

            let applyMutation mutation organism = 
                match mutation with
                    |Some mutation' -> mutation' organism
                    |None -> organism

            [for mutation, organism in 
            List.zip mutationOperations organisms
            do yield applyMutation mutation organism]
            |> Population

    let genericMutateAndPreserveStrategy mutationOperationsSource (Population organisms) = 
            // A generic mutation strategy template

            let mutationOperations  = 
                mutationOperationsSource 
                |> Seq.head
                |> truncateList (List.length organisms) 
                // Handle mutation lists *longer* than organisms
                |> padList None (List.length organisms) 
                // Handle mutation lists *shorter* than organisms by padding with None's

            let applyMutation organisms (mutation, organism) = 
                match mutation with
                    |Some mutation' -> [mutation' organism; organism]@organisms
                    |None -> [organism]@organisms

            List.zip mutationOperations organisms
            |> List.fold applyMutation []
            |> Population


