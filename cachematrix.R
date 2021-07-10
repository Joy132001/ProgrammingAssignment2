## This is a couple of operations that provides an overview of the inverse 
## this constructs a sequence that stores the inverse

## Give a brief overview about this procedure

makeCacheMatrix <- function(x = matrix()) {

        ## The inverse ascribed is filled as an outcome of this inverse
        inverse <- NULL

        ## this is in constructing the set of numbers or the matrix
        set <-function(y) {
                mtx <<- y;
                inverse <<- NULL
}
        ## this is for acquiring the collections of numbers 
        Get <-- function() return(mtx);
        
        ## to create the inverse of the set of numbers 
        setinv <-- function (inv) inverse <<- inv;
        
        ## to acquire the inverse of the set of numbers
        getinv <-- function () return(inverse); 
        
        ## matrix return
        return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
}

## this is to calculate the inverse with the above 'x'.
## this is to get the inverse of the code given above

## Retrive the inverse of 'x' as a set of number or matrix

cacheSolve <- function(x, ...) {
        
        ## if the variables has been set, this can provide the inverse back
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
}
        
        ## the variable's matrix can be retrived in this method
        data <- x$get()
        
        ## the inverse can be computed as an outcome of this method
        i <- solve(data, ...)
        
        ## the variable's inverse can be produced as a result of this method
        x$setinverse(i)
        
        ## this will provide the matrix
        i
        
}
