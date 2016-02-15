namespace organism

    module operations =
        open utility
        open organism.types

        let identityPhenomeFromGenome (Genome genome) =
            // identity mapping of the genome to the phenome
            Phenome(genome) 

        let organismFromGenome phenomeFromGenome genome =
            // Shorthand function for building an organism from a genome
            {genome = genome; phenome = phenomeFromGenome genome}

        let spawnOrganisms genomeSource organismFromGenome numberOfOrganisms  = 
            genomeSource 
            |> Seq.take numberOfOrganisms 
            |> Seq.map organismFromGenome
            |> Seq.toList

        let generationFromOrganisms costFunction organisms = 
              organisms
              |> List.map (withResult costFunction)
              |> Generation
            