## Write a pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there are some 
## benefits to caching the inverse of a matrix rather than compute it repeatedly.

## The function creates a special "matrix" object that can cache its inverse;
library(MASS)
makeCacheMatrix <-function(x = matrix()) {
    # variable and functions are showed below: 
    inversa <- NULL  ## initiating the inverse as NULL
    set <- function(y) {
        x <<- y
        inversa <<- NULL
    }
    get <- function() {x}  ## function to get matrix x 
    setInversa <-function(inversacalculada) {inversa <<- inversacalculada}
    getInversa <-function() {inversa}
    list(set = set, get = get, 
         setInversa = setInversa,
         getInversa = getInversa)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSlove <- function(x,...) {   ## function to get cache data 
    ## Return a matrix that is the inverse of 'x'
    inversa <- x$getInversa()
    if (!is.null(inversa)) {   ## checking weather inverse is null 
        message("getting cached data")
        return(inversa)   ## return the inverse value 
    }
    data <- x$get()
    inversa <- solve(data,...)   ## calculate inverse value 
    x$setInversa(inversa)
    inversa  ## return a matrix that is the inverse of x
}
