namespace organism

    module signaloperations = 
        open utility
        open organism.signaltypes
        open organism.types

        let signalGenomeFromFloatList floatList = 

            if List.length floatList <= 2 then
                (0.0,[NoDelta]) |> SignalGenome
            else

                let signalFromFloats lastElement thisElement = 
                    match thisElement - lastElement with 
                    | 0.0 -> NoDelta
                    | _ -> thisElement - lastElement |> Delta

                let rec signalGenomeFromFloatList' signalList lastElement remainingFloatList =
                    match remainingFloatList with
                    |[]     -> signalList
                    |hd::tl -> let signal = signalFromFloats lastElement hd
                               let updatedSignalList = signal::signalList
                               signalGenomeFromFloatList' updatedSignalList hd tl

                let lastElement = List.head floatList
                let remainingFloatList = List.tail floatList
                
                (floatList.[0],
                 signalGenomeFromFloatList' []  lastElement remainingFloatList
                 |> List.rev)
                 |> SignalGenome

        let floatListFromSignalGenome genome = 

            let floatFromSignal (lastElement, signal) =
                match signal with
                    |Delta d -> lastElement + d
                    |NoDelta -> lastElement

            let rec floatListFromSignalGenome' floatList lastElement remainingSignalList = 
                match remainingSignalList with
                    |[]     -> floatList
                    |hd::tl -> let flt = floatFromSignal (lastElement, hd)
                               let updatedFloatList = flt::floatList
                               floatListFromSignalGenome' updatedFloatList flt tl
            
            match genome with
                |FloatGenome _ -> []
                |SignalGenome (firstSignal, signals) 
                    -> floatListFromSignalGenome' [] firstSignal signals
                       |> List.rev
                    

            

    module operations =
        open utility
        open organism.types

        let identityPhenomeFromGenome (genome) =
            // identity mapping of the genome to the phenome
            match genome with
                |FloatGenome f -> FloatPhenome(f)
                |SignalGenome (s1,s2) -> SignalPhenome(s1,s2) 

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
            