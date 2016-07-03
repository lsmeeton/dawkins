namespace organism

    module signaloperations = 
        open utility
        open organism.signaltypes
        open organism.types
        
        let signalDiffFromFloat tol flt = 
            match tol, flt with
            |tol, flt when (abs flt) < (abs tol) -> NoDelta
            |_ -> flt |> Delta        
            
        let signalFromFloats tol lastElement thisElement = 
            signalDiffFromFloat tol (thisElement - lastElement)

        let signalGenomeFromFloatList tol floatList = 

            let rec signalGenomeFromFloatList' signalList lastElement remainingFloatList =
                    match remainingFloatList with
                    |[]     -> signalList
                    |hd::tl -> let signal = signalFromFloats tol lastElement hd
                               let updatedSignalList = signal::signalList
                               signalGenomeFromFloatList' updatedSignalList hd tl

            match floatList with
            | floatList when List.length floatList < 2 -> EmptySignal |> SignalGenome
            |_ ->

                let lastElement = List.head floatList
                let remainingFloatList = List.tail floatList
                
                (floatList.[0],
                 signalGenomeFromFloatList' []  lastElement remainingFloatList
                 |> List.rev)
                 |> LiveSignal
                 |> SignalGenome

        let floatListFromSignalGenome genome = 
        
            let rec floatListFromSignalGenome' floatList signal = 
                match signal with
                    |reference, [] -> reference::floatList
                    |reference, (Delta s)::tail -> floatListFromSignalGenome' ((reference + s)::floatList) (reference + s, tail)
                    |reference, (NoDelta)::tail -> floatListFromSignalGenome' (reference::floatList) (reference, tail)
            
            match genome with
                |SignalGenome (LiveSignal (reference, signalDiffs))
                    -> floatListFromSignalGenome' [] (reference, signalDiffs)
                       |> List.rev
                |_ -> []
                    

            

    module operations =
        open utility
        open organism.types

        let identityPhenomeFromGenome (genome) =
            // identity mapping of the genome to the phenome
            match genome with
                |FloatGenome f -> FloatPhenome(f)
                |SignalGenome signal -> SignalPhenome signal

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
            