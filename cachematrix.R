## This is a little pseudo-class that wraps a matrix.  It enables the matrix inverse to be calculated
# a single time, caches that value, and then allows that pre-calculated value to be reused as long as the matrix
# value does not change.
#
# Sample Testing code:
# 1) setup matrix initializer
# > m = matrix(c(1,3,2,4), 2, 2)
#
# > m
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4
#
# 2) populate matrix wrapper
# > mObj = makeCacheMatrix(m)
# > mObj$get()
#     [,1] [,2]
# [1,]    1    2
# [2,]    3    4
#
# 3) compute inverse
# > cacheSolve(mObj)
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
#
# 4) compute inverse again, but this time get the cached value returned
# > cacheSolve(mObj)
# getting cached data
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

##########################################################################################################
## makeCacheMatrix
# This function acts as a wrapper for the matrix, providing getter/setter functionality for the matrix itself,
# as well as for the matrix's inverse.  The inverse is not actually calculated herel it is only provided a
# storage location.
#
# Parameters:
# x : a matrix to use as the default value
#
# Returns: a list object consisting of functions used to manipulate the matrix storage
##########################################################################################################
makeCacheMatrix <- function(x = matrix()) {
    #define initial properties for storage
    matrix <- NULL
    matrixInverse <- NULL

    #setter for matrix itself; when a value is assigned, the inverse is cleared out
    set <- function(value) {
        matrix <<- value
        matrixInverse <<- NULL
    }

    # getter for matrix
    get <- function() {
        matrix
    }

    #setter for inverse of matrix
    setInverse <- function(value) {
        matrixInverse <<- value
    }

    #setter for inverse of matrix
    getInverse <- function() {
        matrixInverse
    }

    #using the value passed in, intialize the matrix
    set(x)

    #return collection of functions for manipulating matrix storage as list (pseudo-object)
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse )
}



##########################################################################################################
## cacheSolve
# This function returns the inverse of a matrix, calculating it if necessary, and caching it.
# In the event that the value was already calculated, it will use that pre-calculated value.
#
# Parameters:
# x : a matrix "list object" whose inverse should be returned.
#
# Returns: The inverse of the matrix 'x'
##########################################################################################################
cacheSolve <- function(x, ...) {
    #see if we already have a copy of the inverse calculated

    #first retrieve inverse
    inverse <- x$getInverse()
    #if not null...
    if(!is.null(inverse)) {
        #return it
        message("getting cached data")
        return(inverse)
    }

    #we didn't have an inverse pre-calculated
    #get the matrix and invert it
    data <- x$get()
    inverse <- solve(x$get(), ...)

    #cache the inverse
    x$setInverse(inverse)
    #return the inverse
    inverse
}
