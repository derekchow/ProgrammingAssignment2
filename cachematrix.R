# A pair of functions that cache the inverse of a matrix:
# - makeCacheMatrix
# - cacheSolve

# Note: I have also written unit tests to document verify examples of the usage
# of these functions.  The tests can be found in the tests/test_cachematrix.R
# file and have been written for use with the 'testthat' library (which prints
# out a '.' for each passed test - for convenience, you can run the tests using
# 'source("run_tests.R")' when the working directory is set relative to this
# file.


# This function creates a special "matrix" object that can cache its inverse.
# The return type is actually a list containing functions to:
# - set the value of internal matrix
# - get the value of internal matrix
# - set the value of the inverse
# - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
