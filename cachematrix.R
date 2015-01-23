## rprog-010 Programming Assignment 2
## author: madarp

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    myInverse <- NULL

    ## method to reassign new input matrix, clear the inverse
    set <- function(y) {
        x <<- y
        myInverse <<- NULL
    }
    
    ## method to return input matrix
    get <- function() {x}
    
    ## method to set the matrix inverse
    setInverse <- function(inv) {myInverse <<- inv}
    
    ## method to return the matrix inverse
    getInverse <- function() {myInverse}
    
    ## return the list of object methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## attempt to get an inverse if it exists
    matrixInv <- x$getInverse()
    if (!is.null(matrixInv)) {
        message("getting cached matrix")
        return(matrixInv)
    }
    
    ## inverse was not yet cached.  Get the original input matrix,
    m <- x$get()
    ## compute it's inverse,
    matrixInv <- solve(m, ...)
    ## cache the inverse
    x$setInverse(matrixInv)
    ## and return it.
    matrixInv
}
