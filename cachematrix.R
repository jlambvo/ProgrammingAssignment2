## These functions create a special form of a matrix object that 
## stores the result of an inverse operation as well as the original data.

## The first function is used to create an object that stores a matrix 
## value, and a list of method-like functions to get, modify, and set 
## the matrix data with an external operation.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL ## Clear inverse value flag when initialized
    
    set <- function(y){ ## Method to set original matrix
        x <<- y ## Store a matrix in x
        i <<- NULL ## Clear inverse value flag
    }
    
    get <- function() x ## Method to return original matrix
    setInverse <- function(inverse) i <<- inverse ## Method to set i to inverse matrix
    getInverse <- function() i ## Method to return inverse matrix
    
    list( ## create list of methods
        set=set,
        get=get,
        setInverse=setInverse,
        getInverse=getInverse
    )
}


## This function takes a cached matrix object and performs an
## inversation function, if it does not already exist, and writes
## it back to the original object

cacheSolve <- function(x, ...) {
    
    i <- x$getInverse() ## Get the current inverse matrix from cache
    
    if(!is.null(i)){
        message("Getting cached matrix") ## If it is not null, show message and display
        return(i)
    }
    
    data <- x$get() ## Otherwise get the original matrix from cache
    i <- solve(data) ## Create an inverse of the matrix in i
    x$setInverse(i) ## Call the setInverse method on the input object to cache the inverse matrix
    i ## Display the inverse matrix results
}
