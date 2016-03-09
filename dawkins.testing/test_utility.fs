namespace dawkins.testing.utility

open dawkins
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``Given a List, lst, of length, len`` ()=
    
    let lst = [0..9] // a list of ten elements, the ints 0-9
    let len = List.length lst
    let padding = 0
    

    [<Test>] member x.
     ``truncateList will return lst when desiredLength = len.`` () =
            FsUnit.Assert.AreEqual(utility.truncateList len lst, lst)

    [<Test>] member x.
     ``padList will return lst when desiredLength = len.`` () =
            FsUnit.Assert.AreEqual(utility.padList padding len lst, lst)

    [<Test>] member x.
     ``truncateList will return lst when desiredLength < len.`` () =
            FsUnit.Assert.AreEqual(utility.truncateList (len + 1) lst, lst)

    [<Test>] member x.
     ``padList will return lst when desiredLength > len.`` () =
            FsUnit.Assert.AreEqual(utility.padList padding (len - 1) lst, lst)

    [<Test>] member x.
     ``truncateList will return a shortened list when desiredLength < len.`` () =
            FsUnit.Assert.AreEqual(utility.truncateList (len - 1) lst, [1..9])

    [<Test>] member x.
     ``padList will return an expanded list when desiredLength > len.`` () =
            FsUnit.Assert.AreEqual(utility.padList padding (len + 1) lst, 0::[0..9])



