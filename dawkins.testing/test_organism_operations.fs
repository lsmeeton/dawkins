namespace dawkins.testing.test_organism_operations

open dawkins
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``Given a Genome and Phenome which are identical`` ()=
    let g = organism.types.Genome [|1.0..10.0|]
    let p = organism.types.Phenome [|1.0..10.0|]
    let p' = organism.operations.identityPhenomeFromGenome g

    [<Test>] member x.
     ``the identity maps the Genome to the Phenome.`` () =
            FsUnit.Assert.AreEqual(p,p')



