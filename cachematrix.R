## These functions provide a way to check for a cached version of
## the matrix inversion and if not found, calculate it and cache it.

## This function provides a special "matrix" object that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      matrixInverse <- NULL
      set <- function(y) {
            x <<- y
            matrixInverse <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) matrixInverse <<- solve
      getInverse <- function() matrixInverse
      list(set = set, get = get, 
           setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.  If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      matrixInverse <- x$getInverse()
      if(!is.null(matrixInverse)) {
            message("getting cached data")
            return(matrixInverse)
      }
      data <- x$get()
      matrixInverse <- solve(data, ...)
      x$setInverse(matrixInverse)
      matrixInverse
}
