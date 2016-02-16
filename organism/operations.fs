namespace organism

    module operations =
        open utility
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

        let spawnOrganisms genomeSource organismFromGenome numberOfOrganisms  = 
            genomeSource 
            |> Seq.take numberOfOrganisms 
            |> Seq.map organismFromGenome
            |> Seq.toList

        let generationFromOrganisms organisms = 
              organisms
              |> Population
            