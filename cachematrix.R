## Functions to handle the 'special' matrix,
## containing a cache for its inverse.

## Creates a list of functions to handle the matrix x and
## the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # cache for the inverse matrix
    cacheInv <- NULL
    
    set <- function(newMatrix) {
        x <<- newMatrix
        message("Invalidating the cache...")
        cacheInv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInv <- function(invMatrix) {
        cacheInv <<- invMatrix
    }
    
    getInv <- function() {
        cacheInv
    }
    
    # return the list of functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Retrieves the inverse matrix from the cache, 
## if the cache is not available, computes the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    invMatrix <- x$getInv()
    
    if (is.null(invMatrix))
    {
        # actually compute the inverse matrix
        invMatrix <- solve(x$get(), ...)
        # set the cache in the special matrix
        x$setInv(invMatrix)
    }
    else
    {
        message("Getting cached inverse matrix...")
    }
    
    invMatrix
}
