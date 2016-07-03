namespace organism

    module signaltypes =
        type SignalDelta = 
            |Delta of float
            |NoDelta

        type Signal = 
            // An initial reference value plus a list of deltas
            | LiveSignal of float * SignalDelta list
            | EmptySignal

    module types = 
    
        type Genome = 
            |FloatGenome of float array
            |SignalGenome of signaltypes.Signal
        
        type Phenome = 
            |FloatPhenome of float array
            |SignalPhenome of signaltypes.Signal

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

    
