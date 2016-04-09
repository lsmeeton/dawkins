namespace dawkins.testing.replicate

open dawkins
open NUnit.Framework
open FsUnit

// Useful resources
// https://github.com/dmohl/FsUnit/
// http://www.davesquared.net/2013/03/hello-world-testing-in-fsharp.html

[<TestFixture>]
type ``Given a Population and Mating Function`` ()=
    
    let g1 = organism.types.Genome [|1.0..10.0|]
    let g2 = organism.types.Genome [|11.0..20.0|]

    let o1 = {organism.types.genome = g1; 
              organism.types.phenome = organism.operations.identityPhenomeFromGenome g1;
              organism.types.fitness = organism.types.NoFitness}
    let o2 = {organism.types.genome = g2; 
              organism.types.phenome = organism.operations.identityPhenomeFromGenome g2;
              organism.types.fitness = organism.types.NoFitness}
    let p = organism.types.Population [o1; o2]
    let emptyOrganismList = []
    

    [<Test>] member x.
     ``an empty mating list returns an empty organism list.`` () =
            FsUnit.Assert.AreEqual(replicate.matingPairsFromPopulation p [], [])

    [<Test>] member x.
     ``a mating list containing invalid indices returns an empty organism list.`` () =
            FsUnit.Assert.AreEqual(replicate.matingPairsFromPopulation p [(-1,2)], [])

    [<Test>] member x.
     ``a mating list containing repeated indices returns an organism list with repeated entries.`` () =
            FsUnit.Assert.AreEqual(replicate.matingPairsFromPopulation p [(0,0)], [o1,o1])

    [<Test>] member x.
     ``a mating list containing unique indices returns an organism list with unique entries.`` () =
            FsUnit.Assert.AreEqual(replicate.matingPairsFromPopulation p [(0,1)], [o1,o2])

    [<Test>] member x.
     ``a mating list containing multiple pairs of indices returns an organism list containing multiple pairs``() =
            FsUnit.Assert.AreEqual(replicate.matingPairsFromPopulation p [(0,0);(0,1);(1,1)], [(o2,o2);(o1,o2);(o1,o1)])

    [<Test>] member x.
     ``a mating list containing multiple pairs of indices (including errors) returns an organism list containing multiple pairs``() =
            FsUnit.Assert.AreEqual(replicate.matingPairsFromPopulation p [(0,0);(0,1);(1,2)], [o1,o2;o1,o1])

[<TestFixture>]
type ``Given a generic list of elements`` ()=
    
    let evenInputLst = [1..10]
    let oddInputLst = [1..9]

    let evenOutputLst = List.rev [for i in [1..2..10] do yield (i,i+1)]
    let oddOutputLst = List.rev [for i in [1..2..8] do yield (i,i+1)]
    

    [<Test>] member x.
     ``partnerListElements returns an empty output list when given an empty input list .`` () =
            FsUnit.Assert.AreEqual(replicate.partnerListElements [], [])

    [<Test>] member x.
     ``partnerListElements returns an empty output list when given an input list containing a single element .`` () =
            FsUnit.Assert.AreEqual(replicate.partnerListElements [1], [])

    [<Test>] member x.
     ``partnerListElements returns an output list containing N / 2 pairs when given an input list containing an even N elements .`` () =
            FsUnit.Assert.AreEqual(replicate.partnerListElements evenInputLst, evenOutputLst)

    [<Test>] member x.
     ``partnerListElements returns an output list containing (N-1) / 2 pairs when given an input list containing an odd N elements .`` () =
            FsUnit.Assert.AreEqual(replicate.partnerListElements oddInputLst, oddOutputLst)