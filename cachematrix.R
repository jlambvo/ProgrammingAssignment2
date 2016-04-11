## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL ## Clear inverse value flag
    
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


## Write a short comment describing this function

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