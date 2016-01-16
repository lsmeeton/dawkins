module replicate
    open organism
    open nondeterministic
    
    let binarySplice (genome1 : Genome) (genome2 : Genome) (boolList :  bool list) = 

        //  Need to implement some error checking (on lengths etc.) but not sure how!

        let g1 = (genome1 |> (fun (Genome g1) -> g1))
        let g2 = (genome2 |> (fun (Genome g2) -> g2))
        
        let bl = List.toArray boolList
        
        let binarySplice' = function
            |(x,y,true)  -> x
            |(x,y,false) -> y


        Array.zip3 g1 g2 bl |> Array.map binarySplice' |> Genome

        
    let mateBinarySplice (phenomeFromGenome : (Genome -> Phenome)) (organism1 : Organism) (organism2 : Organism) (boolList :  bool list) = 
        let childGenome = (binarySplice organism1.genome organism2.genome boolList) 
        {genome = childGenome; phenome = phenomeFromGenome childGenome}