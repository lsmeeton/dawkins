module replicate

    open utility
    open inputlegallity
    open organism.types

    let partnerListElements l = 
        let rec partnerListElements' acc l = 
            match l with
                |[] -> acc
                |fst::[] -> acc
                |fst::snd::rest -> partnerListElements' ((fst,snd)::acc) rest
        partnerListElements' [] l

    let binarySplice (FloatGenome g1) (FloatGenome g2) (boolList :  bool list) = 
        if Array.length g1 <> Array.length g2 then
            // This is not ideal behaviour, but condition should never be met ...
            g1 |> FloatGenome
        else
            let boolArray = boolList
                           |> truncateList (Array.length g1)
                           |> padList true (Array.length g1)
                           |> List.toArray
        
            Array.zip3 g1 g2 boolArray
            |> Array.map (fun x -> match x with |(a,b,true) -> a|(a,b,false) -> b)
            |> FloatGenome

        
    let mateBinarySplice costFunction phenomeFromGenome organism1 organism2 boolList = 
        let fitnessFromGenome = phenomeFromGenome >> costFunction
        let childGenome = binarySplice organism1.genome organism2.genome boolList 
        {genome = childGenome; 
         phenome = phenomeFromGenome childGenome;
         fitness = fitnessFromGenome childGenome} 
                                  

    
    let matingPairsFromPopulation (Population organisms) matingPairsByIndex  = 
        // return a list of Organism tuples from population according to 
        // the pairs defined in matingTuples

        let matingPairsFromPopulation' organisms matingPairsByIndex  =
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

    let genericInfinitePairsSource (pairFunction : int list) =
        // Generate an infinite sequence of integer tuples
        // according to pairFunction
        Seq.initInfinite(fun _ -> pairFunction |> partnerListElements)


    let genericReplicateAndReplaceStrategy matingStrategy (Population generation) =
        // Given a matingFunction and generation
        // return a Population 
        generation
        |> matingStrategy
        |> Population

    let genericReplicateAndPreserveStrategy matingStrategy generation = 
        generation
        |> matingStrategy
        |> fun (Population p) -> p @ (generation|>(fun (Population g) -> g))
        |> Population
       

    let _genericMatingStrategy matingPairs matingOperations (Population organisms) = 
        // A generic mating strategy template

        // matingPairs and matingOperations should be the same length
        if List.length matingPairs <> List.length matingOperations then
            IllegalInputLength
        else
            //Some sort of error checking on lengths and sizes etc
            try
                [for matingPair, matingOperation in 
                List.zip matingPairs matingOperations 
                do yield matingOperation organisms.[fst matingPair] organisms.[snd matingPair]]
                //|> List.fold (fun acc o -> match o with |Some o' -> o'::acc |None -> acc) []
                |> Population
                |> ValidInput
            with
                | :? System.ArgumentException -> IllegalContainerIndex

    let genericMatingStrategy matingPairsSource matingOperationsSource organisms = 
        // A generic mating strategy template
        let matingPairs = 
            matingPairsSource 
            |> Seq.head

        let matingOperations = 
            matingOperationsSource 
            |> Seq.head
            |> truncateList (List.length matingPairs)
            |> padList (fun o1 o2 -> o1) (List.length matingPairs)

        //Some sort of error checking on lengths and sizes etc
        
        match _genericMatingStrategy matingPairs matingOperations organisms with
            |ValidInput p -> p
            |IllegalContainerIndex -> raise IllegalContainerIndexError 
            |IllegalInputLength -> raise IllegalInputLengthError 




            