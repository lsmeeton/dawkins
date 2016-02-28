module nondeterministic
// Contains all the non-deterministic elements of the program

    open System

    // random integers

    type RandomBinaryList = RandomBinaryList of int list

    let rec randomSeq (rnd : System.Random) =
        seq {yield rnd.NextDouble()
             yield! randomSeq(rnd)}

    let rec randomBinarySeq (rnd : System.Random) =
        seq {yield rnd.Next(0,2)
             yield! randomBinarySeq(rnd)}

    let generateRandomBinaryList (rnd : System.Random) (length : int) =
        let b = randomBinarySeq rnd
        b |> Seq.take length |> Seq.toList |> RandomBinaryList

    // random bools

    let generateRandomBoolList (rnd : System.Random) (length : int) = 
        let b = randomBinarySeq rnd
        
        let boolFromInt = function
            |0 -> false
            |_ -> true

        b |> Seq.take length |> Seq.map boolFromInt |> Seq.toList;;

    // random floats

    let rec randomFloatSeq (rnd : System.Random) lowerVal upperVal = 
        let valRange = upperVal - lowerVal
        seq {yield ((rnd.NextDouble() * valRange) + lowerVal)
             yield! randomFloatSeq rnd lowerVal upperVal}

    let generateRandomFloatList (rnd : System.Random) lowerVal upperVal length =
        let b = randomFloatSeq rnd lowerVal upperVal
        b |> Seq.take length |> Seq.toList 

    let generateRandomFloatArray (rnd : System.Random) lowerVal upperVal length =
        List.toArray (generateRandomFloatList rnd lowerVal upperVal length)  


    // random shuffle

    let randomlyShuffleList (rnd: System.Random) l = 
        let a = List.toArray l

        let shuffle (rnd: System.Random) a =
            // An in-place random shuffle
            let swap (b: _ []) x y = 
                let tmp = b.[x]
                b.[x] <- b.[y]
                b.[y] <- tmp
            Array.iteri (fun i _ -> swap a i (rnd.Next(i, Array.length a))) a

        shuffle rnd a

        Array.toList a

    //Random number generator
    let rnd = new Random()


