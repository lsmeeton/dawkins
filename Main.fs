// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open nondeterministic
open organism.types
open organism.operations
open replicate


[<EntryPoint>]
let main argv =
    let populationSize = 2
    let genomeLength = 2*4

    let genomeSource = 
        Seq.initInfinite (fun i -> Genome(generateRandomFloatArray rnd genomeLength))

    let identityOrganismFromGenome = organismFromGenome identityPhenomeFromGenome

    let pop = Population( spawnOrganismList genomeSource identityOrganismFromGenome populationSize )

    let g1 = Genome(generateRandomFloatArray rnd genomeLength)
    let g2 = Genome(generateRandomFloatArray rnd genomeLength)

    let o1 = {genome = g1; phenome = identityPhenomeFromGenome g1}
    let o2 = {genome = g2; phenome = identityPhenomeFromGenome g2}

    let randomBoolList = generateRandomBoolList rnd genomeLength

    let child = mateBinarySplice identityPhenomeFromGenome o1 o2 randomBoolList

    let p = Population([o1; o2; child])

    printfn "Population: %A\n" pop

    0 // return an integer exit code
