module replicate

    open organism.types
    
    let binarySplice (Genome g1) (Genome g2) (boolList :  bool list) = 
        //  Need to implement some error checking (on lengths etc.) but not sure how!
        Array.zip3 g1 g2 (List.toArray boolList) 
        |> Array.map (fun x -> match x with |(a,b,true) -> a|(a,b,false) -> b)
        |> Genome

        
    let mateBinarySplice phenomeFromGenome organism1 organism2 boolList = 
        let childGenome = (binarySplice organism1.genome organism2.genome boolList) 
        {genome = childGenome; phenome = phenomeFromGenome childGenome}

    
    let matingPairsFromPopulation (Population population) (matingPairsByIndex : (int * int) list) = 
        // return a list of Organism tuples from population according to 
        // the pairs defined in matingTuples
        let matingPairsFromPopulation' (population : Organism list) (matingPairsByIndex : (int * int) list) =
            let getOrganism' (pop : Organism list) index = 
                try
                    Some pop.[index]
                with
                    | :? System.ArgumentException -> None

            let addPairsToList population accOrgList pairIndices =
                match (getOrganism' population (fst pairIndices), getOrganism' population (snd pairIndices)) with
                    |(Some o1, Some o2) -> (o1,o2)::accOrgList 
                    |(_,_) -> accOrgList

            let rec matchPairs (pop : Organism list) (acc : (Organism * Organism) list) (mpbi : (int * int) list) =
                match mpbi with
                    |[] -> acc
                    |_ -> matchPairs pop (addPairsToList pop acc (List.head mpbi)) (List.tail mpbi) 

            let accOrgList = []

            matchPairs population accOrgList matingPairsByIndex
        
        match population with
            |[] -> []
            |_ -> matingPairsFromPopulation' population matingPairsByIndex






            