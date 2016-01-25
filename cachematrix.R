## These functions cache a given matrix and calculate an inverse on that
## matrix.  When an inverse is calculated, the result is cached so
## future calculations simply return the cached value.

## Save the given matrix in a returned class.
## The class has member functions for getting the originally given matrix,
## setting a cached inverse matrix, and getting that cached inverse matrix.
## All these class functions are used internally.
##
## The original matrix and the cached inverse matrix are both stored in
## the environment captured when makeCacheMatrix was called.  So, each
## call of makeCacheMatrix has its own set of cached variables to work with.
## Any time a function is called, the variables created in it, including
## passed arguments, are captured in a new private environment.  When
## a function defined in makeCacheMatrix refers to one of these variables,
## the cached value is used.  Operators like <<- refer to the environment
## rather than creating new variables when the function is called.

makeCacheMatrix <- function(x = matrix()) {
    myInverse <- NULL
    get <- function() x
    setInverse <- function(newInverse) myInverse <<- newInverse
    getInverse <- function() myInverse
	list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## Provide an object created by makeCacheMatrix().
## The return value will be an inverse matrix for the original matrix
## provided to makeCacheMatrix().
## A cached value may be used to help with performance.

cacheSolve <- function(x, ...) {
	newInverse <- x$getInverse()
	if (!is.null(newInverse)) {
        message("getting cached data")
	    return(newInverse)
	}
	myOriginal <- x$get()
	newInverse <- solve(myOriginal)
	x$setInverse(newInverse)
	newInverse
}
