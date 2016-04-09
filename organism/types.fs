namespace organism

    module signaltypes =
        type Signal = 
            |Delta of float
            |NoDelta

    module types = 
    
        type Genome = 
            |FloatGenome of float array
            |SignalGenome of float * signaltypes.Signal list
        
        type Phenome = 
            |FloatPhenome of float array
            |SignalPhenome of float * signaltypes.Signal list

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

    
