module inputlegallity
    
    open System
    open System.Collections.Generic

    type InputLegallity<'a> =
        //Type to handle illegal input state
        //Intended for use when there is scope for ill-defined or illegal behaviour
        //when typesafe but bad input is used
        |ValidInput of 'a
        |IllegalInputLength 
        |IllegalContainerIndex

    exception IllegalInputLengthError
    exception IllegalContainerIndexError