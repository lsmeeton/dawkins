// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

open nondeterministic
open utility
open organism.types
open organism.operations
open replicate
open naturalselection


[<EntryPoint>]
let main argv =

    // Prepare population
    // Optimise two-dimensional points to sit at the origin
    // Genome -> cartesian coords [|x;y|]
    // Phenome -> spherical polar coords [|r;theta|]

    let populationSize = 30
    let genomeLength = 2
    let nGenerations = 10

    let sphericalPolarFromCartesian (Genome cartesianGenome) = 
        let r = cartesianGenome
                |>Array.fold (fun acc elem -> acc + elem * elem) 0.
                |>Math.Sqrt 
        let theta = Math.Atan2 (cartesianGenome.[1], cartesianGenome.[0])

        [|r;theta|]
        |> Phenome

    // Prepare matingStrategy
    let pairsSource =
        Seq.initInfinite(fun _ -> randomlyShuffleList rnd [0..populationSize-1]
                                                |> partnerListElements)

    let matingFunctionSource = 
        let matingBinarySplice' = mateBinarySplice (fun p -> NoFitness) (fun (Genome g) -> Phenome g)
        let perumteArgs f boolList o1 o2 = f o1 o2 boolList
        let nMatingPairs = populationSize / 2
        Seq.initInfinite(fun _ -> [for __ in [1 .. nMatingPairs] do yield generateRandomBoolList rnd genomeLength]
                                  |> List.map (perumteArgs matingBinarySplice'))

    let matingStrategy = genericReplicateAndPreserveStrategy (genericMatingStrategy pairsSource matingFunctionSource)

    // Prepare mutationStrategy
    let mutationStrategy = fun x -> x

    // Prepare costFunction
    let noFitnessCostFunction (Phenome p) =
        NoFitness 

     // A toy fitness function for debugging and simple testing!
    let firstElementFitness (Phenome p) = 
        p.[0] |> Fitness


    // Prepate logger
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

    // Prepare initial generation
    let genomeSource = 
        Seq.initInfinite (fun i -> Genome(generateRandomFloatArray rnd genomeLength))

    // genome to phenome mapping
    let identityOrganismFromGenome = organismFromGenome noFitnessCostFunction identityPhenomeFromGenome

    let intialGeneration = spawnOrganisms genomeSource identityOrganismFromGenome populationSize
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

    let gp = generationPropagator mostFitLogger matingStrategy mutationStrategy selectionStrategy firstElementFitness 

    let generations = 
        [1..nGenerations]
        |> List.fold (fun acc p -> (gp (List.head acc) p)::acc) [intialGeneration]


    0 // return an integer exit code
