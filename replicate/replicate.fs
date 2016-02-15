module replicate

    open organism.types
    
    let partnerListElements l = 
        let rec partnerListElements' acc l = 
            match l with
                |[] -> acc
                |fst::[] -> acc
                |fst::snd::rest -> partnerListElements' ((fst,snd)::acc) rest
        partnerListElements' [] l

    let binarySplice (Genome g1) (Genome g2) (boolList :  bool list) = 
        let binarySplice' g1 g2 boolList =
            Array.zip3 g1 g2 (List.toArray boolList) 
            |> Array.map (fun x -> match x with |(a,b,true) -> a|(a,b,false) -> b)
            |> Genome
        try 
            binarySplice' g1 g2 boolList |> Some 
        with
            | :? System.ArgumentException -> None

        
    let mateBinarySplice phenomeFromGenome organism1 organism2 boolList = 
        match binarySplice organism1.genome organism2.genome boolList with
            |Some childGenome -> {genome = childGenome; phenome = phenomeFromGenome childGenome} |> Some
            |None -> None

    
    let matingPairsFromPopulation (Generation generation) matingPairsByIndex  = 
        // return a list of Organism tuples from population according to 
        // the pairs defined in matingTuples

        let organisms = generation |> List.map (fun x -> fst x)

        let matingPairsFromPopulation' (organisms : Organism list) (matingPairsByIndex : (int * int) list) =
            let getOrganism' (orgs : Organism list) index = 
                try
                    Some orgs.[index]
                with
                    | :? System.ArgumentException -> None

            let addPairsToList organisms matingPairs pairIndices =
                match (getOrganism' organisms (fst pairIndices), getOrganism' organisms (snd pairIndices)) with
                    |(Some o1, Some o2) -> (o1,o2)::matingPairs 
                    |(_,_) -> matingPairs

            let rec matchPairs organisms matingPairs matingPairsByIndex =
                match matingPairsByIndex with
                    |[] -> matingPairs
                    |_ -> matchPairs organisms (addPairsToList organisms matingPairs (List.head matingPairsByIndex)) (List.tail matingPairsByIndex) 

            let accOrgList = []

            matchPairs organisms accOrgList matingPairsByIndex

        match organisms with
            |[] -> []
            |_ -> matingPairsFromPopulation' organisms matingPairsByIndex






            