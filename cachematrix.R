## Functions for calculating the inverse of a matrix (caching the
## results where possible).


## Stores a matrix along with functions for getting and setting its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of a makeCacheMatrix matrix, caching it on the
## first calculation and retrieving from teh cache on subsequent
## calls.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
