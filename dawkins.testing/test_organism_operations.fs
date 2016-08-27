namespace dawkins.testing.test_organism_operations

open dawkins
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``Given a Genome and Phenome which are identical`` ()=
    let g = organism.types.FloatGenome [|1.0..10.0|]
    let p = organism.types.FloatPhenome [|1.0..10.0|]
    let p' = organism.operations.identityPhenomeFromGenome g

    [<Test>] member x.
     ``the identity maps the Genome to the Phenome.`` () =
            FsUnit.Assert.AreEqual(p,p')

[<TestFixture>]
type ``Given a sequence of Genomes and a genome-to-organism mapping`` ()=
    let nOrganisms = 10
    let gSeq = [for i in [1..nOrganisms] do yield organism.types.FloatGenome [|float i|]]
               |> List.toSeq
    let oFromG = fun g -> {organism.types.genome = g; 
                           organism.types.phenome = organism.operations.identityPhenomeFromGenome g;
                           organism.types.fitness = organism.types.NoFitness}
    let oLst = [for i in [1..nOrganisms] do yield {organism.types.genome = organism.types.FloatGenome [|float i|]; 
                                                   organism.types.phenome = organism.types.FloatPhenome [|float i|];
                                                   organism.types.fitness = organism.types.NoFitness}]
    let oLstShorter = [for i in [1..(nOrganisms-1)] do yield {organism.types.genome = organism.types.FloatGenome [|float i|]; 
                                                   organism.types.phenome = organism.types.FloatPhenome [|float i|];
                                                   organism.types.fitness = organism.types.NoFitness}]

    [<Test>] member x.
     ``spawnOrganisms returns an empty list when number of organisms requested is zero.`` () =
            FsUnit.Assert.AreEqual(organism.operations.spawnOrganisms gSeq oFromG 0, [])

    [<Test>] member x.
     ``spawnOrganisms returns an organism list of correct length and composition when number of organisms requested is equal to the sequence length.`` () =
            FsUnit.Assert.AreEqual(organism.operations.spawnOrganisms gSeq oFromG nOrganisms, oLst)

    [<Test>] member x.
     ``spawnOrganisms returns an organism list of correct length and composition when number of organisms requested is longer than the sequence length.`` () =
            FsUnit.Assert.AreEqual(organism.operations.spawnOrganisms gSeq oFromG (nOrganisms + 1), oLst)

    [<Test>] member x.
     ``spawnOrganisms returns an organism list of correct length and composition when number of organisms requested is shorter than the sequence length.`` () =
            FsUnit.Assert.AreEqual(organism.operations.spawnOrganisms gSeq oFromG (nOrganisms - 1), oLstShorter)



