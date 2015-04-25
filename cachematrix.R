## This function returns matrix inverse, uses a list to store
## a set of functions used to calc the inverse matrix
## Calc'd inverse matrix is stored in cache.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        getmatrix <- function() x
        setinverse <- function(solution) cachedInverse <<- solution
        getinverse <- function() cachedInverse
        list(getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns inverse of the matrix taken from cache.
## if cache is empty, this function will calc the inverse
## matrix, store it in cache and return it

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached data .... ")
                return(inverse)
        }
        MATRIX <- x$getmatrix()
        inverse <- solve(MATRIX, ...)
        x$setinverse(inverse)
        inverse
}