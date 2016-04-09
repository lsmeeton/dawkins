namespace organism

    module operations =
        open utility
        open inputlegallity
        open organism.types

        let identityPhenomeFromGenome (Genome genome) =
            // identity mapping of the genome to the phenome
            Phenome(genome) 

        let organismFromGenome costFunction phenomeFromGenome genome =
            // Shorthand function for building an organism from a genome
            let fitnessFromGenome = phenomeFromGenome >> costFunction
            {genome = genome; 
             phenome = phenomeFromGenome genome;
             fitness = fitnessFromGenome genome}

        let _spawnOrganisms (genomeSource:seq<Genome>) (organismFromGenome:Genome -> Organism) (numberOfOrganisms:int)  = 
            try
                genomeSource 
                |> Seq.take numberOfOrganisms 
                |> Seq.map organismFromGenome
                |> Seq.toList
                |> ValidInput
            with
                | :? System.InvalidOperationException -> IllegalInputLength

        let rec spawnOrganisms (genomeSource:seq<Genome>) (organismFromGenome:Genome -> Organism) (numberOfOrganisms:int)  = 
            match _spawnOrganisms genomeSource organismFromGenome numberOfOrganisms with
                |ValidInput x -> x
                |IllegalInputLength -> spawnOrganisms genomeSource organismFromGenome (numberOfOrganisms - 1)

        let generationFromOrganisms organisms = 
              organisms
              |> Population
            