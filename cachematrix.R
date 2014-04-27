## The functions below implement a special "matrix". This "matrix" caches its
## inverse matrix. If contents of a matrix are not changing, it may make sense
## to cache its inverse matrix so that when the inverse is needed again, it can
## be looked up in the cache rather than recomputed.


## This function creates a special "matrix" that caches its inverse. This
## "matrix" is a list containing the functions to:
##     - set the matrix;
##     - get the matrix;
##     - set the inverse matrix;
##     - get the inverse matrix;
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    setMatrix <- function( newMatrix ) {
        x <<- newMatrix
        inverseMatrix <<- NULL
    }
    
    getMatrix <- function() x
    
    setInverseMatrix <- function(newInverseMatrix) {
        inverseMatrix <<- newInverseMatrix
    }
    
    getInverseMatrix <- function() inverseMatrix
    
    list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix
        )
}


## This function calculates the inverse matrix of the special "matrix" created
## with the above function. 
## If the inverse matrix has already been calculated for the specified special
## "matrix", then the inverse matrix will be retrieved from the cache and the
## calculation will be skipped. Otherwise, the inverse matrix will be
## and saved in the cache.
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
        
    internalMatrix <- x$getMatrix()
    inverseMatrix <- solve(internalMatrix)
    x$setInverseMatrix( inverseMatrix )
    inverseMatrix
}
