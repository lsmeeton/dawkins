namespace dawkins.testing.replicate

open dawkins
open NUnit.Framework
open FsUnit

// Useful resources
// https://github.com/dmohl/FsUnit/
// http://www.davesquared.net/2013/03/hello-world-testing-in-fsharp.html

[<TestFixture>]
type ``Given a Population and Mating Function`` ()=
    
    let g1 = organism.types.FloatGenome [|1.0..10.0|]
    let g2 = organism.types.FloatGenome [|11.0..20.0|]

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

[<TestFixture>]
type ``Given two Genomes and a list of bools`` ()=
    
    let g1 = organism.types.Genome [|1.0..10.0|]
    let g2 = organism.types.Genome [|11.0..20.0|]
    let g3 = organism.types.Genome [|11.0..19.0|]

    let boolList1 = [for i in [1..5] do yield true, false]
                    |>List.fold (fun acc x -> (fst x)::(snd x)::acc) []

    let boolList2 = [for i in [1..4] do yield true, false]
                    |>List.fold (fun acc x -> (fst x)::(snd x)::acc) [true]

    let boolList3 = [for i in [1..5] do yield false, true]
                    |>List.fold (fun acc x -> (fst x)::(snd x)::acc) [false]

    let out_g1 = organism.types.Genome [|1.0; 12.0; 3.0; 14.0; 5.0; 16.0; 7.0; 18.0; 9.0; 20.0|]
    let out_g2 = organism.types.Genome [|1.0; 2.0; 13.0; 4.0; 15.0; 6.0; 17.0; 8.0; 19.0; 10.0|]
    

    [<Test>] member x.
     ``binarysplice returns a correct genome if both the input genomes and list of bools are legal input`` () =
            FsUnit.Assert.AreEqual(replicate.binarySplice g1 g2 boolList1 , out_g1)

    [<Test>] member x.
     ``binarysplice returns the first input genome if the first input genome is shorter than the second`` () =
            FsUnit.Assert.AreEqual(replicate.binarySplice g3 g2 boolList1 , g3)

    [<Test>] member x.
     ``binarysplice returns the first input genome if the first input genome is longer than the second`` () =
            FsUnit.Assert.AreEqual(replicate.binarySplice g1 g3 boolList1 , g1)

    [<Test>] member x.
     ``binarysplice returns a correct genome if the bool list is shorter than the input genomes`` () =
            FsUnit.Assert.AreEqual(replicate.binarySplice g1 g2 boolList2, out_g2)

    [<Test>] member x.
     ``binarysplice returns a correct genome if the bool list is longer than the input genomes`` () =
            FsUnit.Assert.AreEqual(replicate.binarySplice g1 g2 boolList3, out_g1)

[<TestFixture>]
type ``Given a source of mating pairs, a source of mating operations and a population`` ()=
    
    let g1 = organism.types.Genome [|1.0|]
    let g2 = organism.types.Genome [|2.0|]
    let g3 = organism.types.Genome [|3.0|]

    let o1 = {organism.types.genome = g1;
              organism.types.phenome = organism.operations.identityPhenomeFromGenome g1;
              organism.types.fitness = organism.types.NoFitness}

    let o2 = {organism.types.genome = g2;
              organism.types.phenome = organism.operations.identityPhenomeFromGenome g2;
              organism.types.fitness = organism.types.NoFitness}

    let o3 = {organism.types.genome = g3;
              organism.types.phenome = organism.operations.identityPhenomeFromGenome g3;
              organism.types.fitness = organism.types.NoFitness}

    // p is a population of three organisms
    let p = [o1; o2; o3]
            |> organism.types.Population

    // matingListSource is a sequence which specifies the mating partners
    let matingList = [(0,1);(0,2)]
    let matingListSource = [matingList] |> List.toSeq

    let illegalMatingListSource = [[(0,1);(0,10)]] |> List.toSeq

    // matingOperationsSource is a sequence of mating operations
    let fstMatingOperation = fun o1 o2 -> o1
    let sndMatingOperation = fun o1 o2 -> o2

    let matingOperationsList = [fstMatingOperation; sndMatingOperation]
    let matingOperationSource = [matingOperationsList] |> List.toSeq

    let out_p1 = [o1; o3]
                 |> organism.types.Population
    

    [<Test>] member x.
     ``genericMatingStrategy returns a correct population if the input mating pairs, mating sources and population are legal input`` () =
            replicate.genericMatingStrategy matingListSource matingOperationSource p 
            |> should equal out_p1

    [<Test>] member x.
     ``genericMatingStrategy raises an IllegalContainerIndexError exception if a mating pair index points outside the population`` () =
            (fun () -> replicate.genericMatingStrategy illegalMatingListSource matingOperationSource p |> ignore)
            |> should throw typeof<inputlegallity.IllegalContainerIndexError>