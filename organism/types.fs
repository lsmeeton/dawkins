namespace organism

    module types = 
    
        type Genome = 
            Genome of float array
        
        type Phenome = 
            Phenome of float array

        type Organism =
            // A basic organism, contains just a genome 
            // and a phenome
            {genome : Genome
             phenome : Phenome}
        
        type Fitness = 
            Fitness of float

        type Generation = 
            Generation of (Organism * Fitness) list

    
