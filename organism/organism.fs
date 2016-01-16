module organism

    
    type Genome = Genome of float array
        
    type Phenome = Phenome of float array

    type Organism =
        // A basic organism, contains just a genome 
        // and a phenome
        {genome : Genome
         phenome : Phenome}

    let IdentityPhenomeFromGenome (Genome genome) =
        Phenome(genome) 


