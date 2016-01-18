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