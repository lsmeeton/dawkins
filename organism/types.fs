namespace organism

    module types = 
    
        type Genome = 
            Genome of float array
        
        type Phenome = 
            Phenome of float array

        type Fitness = 
            | NoFitness
            | Fitness of float

        type Organism =
            // A basic organism, contains just a genome, 
            // phenome and fitness
            {genome : Genome
             phenome : Phenome
             fitness : Fitness}
        
        type Population = 
            Population of Organism list

    
