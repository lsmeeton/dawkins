// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO

open nondeterministic
open utility
open organism.types
open organism.signaltypes
open organism.operations
open organism.signaloperations
open replicate
open mutate
open naturalselection


[<EntryPoint>]
let main argv =

    // Prepare population
    // Find the optimal match of a flattened signal to an original
    // Genome -> SignalGenome
    // Phenome -> identity mapping to genome

    let populationSize = 30
    let genomeLength = 1024
    let nGenerations = 100

    // import inputSignal from CSV
    let inputSignal = File.ReadAllLines("C:/Users/Lewis/Scratch/NoisySignalData.csv") 
                      // ignore the headers
                      |> Array.toList |> List.tail 
                      // split the string on commas
                      |> List.map (fun (t : string )-> t.Split [|','|]) 
                      // convert to float
                      |> List.map (fun x -> x.[1] |> float)

    // instantiate initial genome
    let initialGenome = signalGenomeFromFloatList inputSignal

    printf "%A" initialGenome

    (*
    // Prepare matingStrategy
    let pairsSource = genericInfinitePairsSource <| randomlyShuffleList rnd [0..populationSize-1]

    let matingFunctionSource = 
        let matingBinarySplice' = mateBinarySplice (fun p -> NoFitness) sphericalPolarFromCartesian
        let perumteArgs f boolList o1 o2 = f o1 o2 boolList
        let nMatingPairs = populationSize / 2
        Seq.initInfinite(fun _ -> [for __ in [1 .. nMatingPairs] do yield generateRandomBoolList rnd 0.5 genomeLength]
                                  |> List.map (perumteArgs matingBinarySplice'))

    let matingStrategy = genericReplicateAndPreserveStrategy (genericMatingStrategy pairsSource matingFunctionSource)

    // Prepare costFunction
    let noFitnessCostFunction (FloatPhenome p) =
        NoFitness 

     // A toy fitness function for debugging and simple testing!
    let firstElementFitness (FloatPhenome p) = 
        p.[0] |> Fitness

    // Prepare mutationStrategy

    let mutationProbability = 0.5
    
    let mutationSource = 
        Seq.initInfinite(fun _ -> [for __ in [1..populationSize*2] do yield generateRandomFloatArray rnd -0.05 0.05 genomeLength]
                                  |> List.map (floatStretchMutation sphericalPolarFromCartesian noFitnessCostFunction))
    let mutationProbabilitySource = 
        Seq.initInfinite(fun _ -> generateRandomBoolList rnd mutationProbability (populationSize * 2))

    let mutationOperationSource = 
        Seq.initInfinite(fun _ -> genericMutationOperation mutationProbabilitySource mutationSource)


    let mutationStrategy = genericMutateAndPreserveStrategy mutationOperationSource
    *)

    // Prepare logger
    let simpleLogger generation =
         printfn "Generation: %A\n" generation

    let fitnessLogger (Population generation) = 
        for o in generation do
            match o.fitness with
                |NoFitness -> printfn "Fitness not evaluated"
                |(Fitness f) -> printfn "Fitness: %2.6f" f

    let mostFitLogger (Population generation) generationNo = 
        printfn "Generation: %d\n==============" generationNo

        for element in [0..2] do
            match generation.[element].fitness with
                |NoFitness -> printfn "Fitness not evaluated"
                |(Fitness f) -> printfn "Fitness: %2.6f" f


    let generationPropertiesLogger (Population generation) (generationNo : int) = 

        let getFitness organism = 
            match organism.fitness with 
            |Fitness f -> f
            |NoFitness -> 0.

        let peak = generation.[0] |> getFitness

        let mean = 
            generation 
            |> List.fold (fun acc o -> (getFitness o) + acc) 0.0
            |> fun f -> f / (float (List.length generation))

        let g = generation.[0].genome
                |> fun (FloatGenome g) -> g

        let x, y = g.[0], g.[1]

        let p = generation.[0].phenome
                |> fun (FloatPhenome p) -> p

        let r, theta = p.[0], p.[1]

        if generationNo % 1 = 0 then 
            printfn "let Gen=%d;; let Peak=%2.6f;; let Mean=%2.6f;; let g = [|%2.6f; %2.6f|];; let p = [|%2.6f; %2.6f|];;" generationNo peak mean x y r theta
            printfn "let fitness = %A" [for o in generation do yield o.fitness]

    let radialLogger (csvFileStream:StreamWriter) (Population generation) generationNo = 
        for o in generation do 
            let ge = o.genome
                     |> fun (FloatGenome g) -> g
            csvFileStream.WriteLine(sprintf "%2.6f, %2.6f, %d" ge.[0] ge.[1] generationNo)

        generationPropertiesLogger (generation |> Population) generationNo

    use csvFileStream = new StreamWriter("plot.csv")
    csvFileStream.WriteLine("x, y, n")
    (*
    // Prepare initial generation
    let genomeSource = 
        Seq.initInfinite (fun i -> FloatGenome(generateRandomFloatArray rnd -1.0 1.0 genomeLength))

    // genome to organism mapping
    let identityOrganismFromGenome = organismFromGenome noFitnessCostFunction identityPhenomeFromGenome

    let organismFromGenome = organismFromGenome noFitnessCostFunction sphericalPolarFromCartesian

    let intialGeneration = spawnOrganisms genomeSource organismFromGenome populationSize
                           |> Population

    // Prepare selectionStrategy
    let selectionStrategy generation = 
        toothAndClaw generation populationSize


    // Log initial generation
    fitnessLogger intialGeneration

    
    // Prepare population propogator operator
    let generationPropagator logger matingStrategy mutationStrategy selectionStrategy costFunction generation generationNo = 
        let applyCostFunction costFunction generation = 
            generation
            |> fun (Population g )-> g
            |> List.map (fun o -> match o.fitness with 
                                    | NoFitness -> {genome = o.genome;
                                                    phenome = o.phenome;
                                                    fitness = costFunction o.phenome}
                                    | _ -> o)
            |> Population

        let nextGeneration = 
            generation
            |> matingStrategy
            |> mutationStrategy
            |> applyCostFunction costFunction
            |> selectionStrategy
        
        logger nextGeneration generationNo

        nextGeneration 

    // Perform genetic algorithm calculations

    let gp = generationPropagator (radialLogger csvFileStream) matingStrategy mutationStrategy selectionStrategy firstElementFitness 

    let generations = 
        [1..nGenerations]
        |> List.fold (fun acc p -> (gp (List.head acc) p)::acc) [intialGeneration]
    *)

    csvFileStream.Close()

    0 // return an integer exit code
