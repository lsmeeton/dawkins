// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open nondeterministic
open organism
open replicate


[<EntryPoint>]
let main argv =
    let genomeLength = 2*4

    let g1 = Genome(generateRandomFloatArray rnd genomeLength)
    let g2 = Genome(generateRandomFloatArray rnd genomeLength)

    let o1 = {genome = g1; phenome = IdentityPhenomeFromGenome g1}
    let o2 = {genome = g2; phenome = IdentityPhenomeFromGenome g2}

    let randomBoolList = generateRandomBoolList rnd genomeLength

    let child = mateBinarySplice IdentityPhenomeFromGenome o1 o2 randomBoolList

    printfn "Organism 1: %A\n" (o1.genome |> (fun (Genome g) -> g))
    printfn "Organism 2: %A\n" (o2.genome |> (fun (Genome g) -> g))
    printfn "Bool List : %A\n" randomBoolList
    printfn "Child     : %A\n" (child.genome |> (fun (Genome g) -> g))

    0 // return an integer exit code
