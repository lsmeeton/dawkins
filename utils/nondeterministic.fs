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

    let rec randomFloatSeq (rnd : System.Random) = 
        seq {yield rnd.NextDouble()
             yield! randomFloatSeq(rnd)}

    let generateRandomFloatList (rnd : System.Random) (length : int) =
        let b = randomFloatSeq rnd
        b |> Seq.take length |> Seq.toList 

    let generateRandomFloatArray (rnd : System.Random) (length : int) =
        List.toArray (generateRandomFloatList rnd length)  


    //Random number generator
    let rnd = new Random()


