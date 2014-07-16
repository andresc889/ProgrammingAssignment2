## This file defines two functions that allow the user to create a matrix and
## find its inverse with the ability of caching the result in order to speed up
## code that requires the inverse of the matrix more than once.
##

## makeCacheMatrix(x)
##
## This function takes a square matrix x and returns a list with four functions
## that allows the user to manipulate/read this matrix:
##
## - set(y): allows the user to change the matrix.
## - get(): returns the matrix.
## - setInv(inv): stores the inverse inv of the matrix.
## - getInv(): returns the inverse of the matrix if it is set or NULL otherwise.
##

makeCacheMatrix <- function(x = matrix()) {
    # i will contain the cached inverse of x
    i <- NULL
    
    set <- function(y) {
        x <<- y
    }
    
    get <- function() {
        x
    }
    
    setInv <- function(inv) {
        i <<- inv
    }
    
    getInv <- function() {
        i
    }
    
    # Return a list of these functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve(x, ...)
##
## This function takes a list x created with the makeCacheMatrix() function. If
## x$getInv() returns NULL, the inverse matrix is calculated and cached using
## x$setInv(). In all cases, x$getInv() is returned (the cached inverse).
## Extra arguments (...) are passed directly to solve(). It is recommended that
## not extra arguments are passed in order to calculate the inverse.
##

cacheSolve <- function(x, ...) {
    # Calculate x's inverse if it has not been calculated before
    if (is.null(x$getInv())) {
        x$setInv(solve(x$get(), ...))
    }
    
    # Return the stored inverse
    x$getInv()
}