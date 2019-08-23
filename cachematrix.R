## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some bene-
## fit to caching the inverse of a matrix rather than compute it repeatedly. 
## Here are described a  pair of functions that compute and cache the inverse of 
## a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can
## compute and cache its inverse matrix in the parent environment.

makeCacheMatrix <- function(x = matrix()) { 
    inv.x <- NULL
    set <- function(y) {
        x <<- y
        inv.x <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv.x <<- inverse
    getInverse <- function() inv.x
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" retur-
## ned by makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache, avoiding to compute it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of "x"
    inv.x <- x$getInverse() 
    if (!is.null(inv.x)) {
        message("getting cached data")
        return(inv.x)
    }
    m <- x$get()
    inv.x <- solve(m)
    x$setInverse(inv.x)
    inv.x
}