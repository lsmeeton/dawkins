module utility 
    
    //http://fsharpforfunandprofit.com/posts/recipe-part3/#comment-1513335278

    // make a tuple from the original value and the value with a function applied to it
    let withResult f a = a, (f a)
    // apply a function to the first part of the tuple, retaining the secondt part
    let mapFirst f (a,b) = (f a), b
    // apply a function to the second part of the tuple, retaining the first part
    let mapSecond f (a,b) = a, (f b)
    // apply a rop function to the first part of the tuple. If the result is success, the success case is a new tuple 

    let rec truncateList desiredLength lst =
        match desiredLength - List.length lst with
            |x when x < 0 -> truncateList desiredLength (List.tail lst)
            |_ -> lst

    let rec padList padding desiredLength lst = 
        match desiredLength - List.length lst with
            |x when x > 0 -> padList padding desiredLength (padding::lst)
            |_ -> lst

