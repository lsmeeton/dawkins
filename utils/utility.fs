module utility 
    
    //http://fsharpforfunandprofit.com/posts/recipe-part3/#comment-1513335278

    // make a tuple from the original value and the value with a function applied to it
    let withResult f a = a, (f a)
    // apply a function to the first part of the tuple, retaining the secondt part
    let mapFirst f (a,b) = (f a), b
    // apply a function to the second part of the tuple, retaining the first part
    let mapSecond f (a,b) = a, (f b)
    // apply a rop function to the first part of the tuple. If the result is success, the success case is a new tuple 
