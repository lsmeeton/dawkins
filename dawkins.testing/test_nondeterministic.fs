module test_nondeterministic

open dawkins
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``Given a generator of random bool lists`` ()=
    
    let len = 1000
    let rndBoolList = nondeterministic.generateRandomBoolList nondeterministic.rnd 

    [<Test>] member x.
     ``generateRandomBoolList will return all false when fractionTrue <= 0`` () =
            FsUnit.Assert.AreEqual(rndBoolList 0.0 len , [for _ in [1..len] do yield false])

    [<Test>] member x.
     ``generateRandomBoolList will return all true when fractionTrue >= 1`` () =
            FsUnit.Assert.AreEqual(rndBoolList 1.0 len , [for _ in [1..len] do yield true])