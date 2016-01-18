module organism.types
    
    type Genome = 
        Genome of float array
        
    type Phenome = 
        Phenome of float array

    type Organism =
        // A basic organism, contains just a genome 
        // and a phenome
        {genome : Genome
         phenome : Phenome}

    type Population = 
        Population of Organism list

    
