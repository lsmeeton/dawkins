namespace organism

    module operations =

        open organism.types

        let identityPhenomeFromGenome (Genome genome) =
            // identity mapping of the genome to the phenome
            Phenome(genome) 

        let organismFromGenome (phenomeFromGenome : (Genome -> Phenome)) (genome : Genome) =
            // Shorthand function for building an organism from a genome
            {genome = genome; phenome = phenomeFromGenome genome}

        let spawnOrganismList (genomeSource : (seq<Genome>)) (organismFromGenome : (Genome -> Organism)) (numberOfOrganisms : int)  = 
            genomeSource 
            |> Seq.take numberOfOrganisms 
            |> Seq.map organismFromGenome
            |> Seq.toList

