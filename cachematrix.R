## cachematrix.R
## 
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. This file contains a pair of functions to do so.
## The R function solve() is used to calculate the inverse
## 
## Contents:
##      makeCacheMatrix
##      cacheSolve
##      makeCacheMatrix2
##
## Implementation notes: this pair of functions is constructed precisely
##   	as requested by the Coursera assignment, despite some concerns
##      about the functions being prone to misuse.
##      See https://class.coursera.org/rprog-012/forum/thread?thread_id=528
## 
## Sample use:
## 		cacheMat <- makeCacheMatrix(mat)
## 		inverse <- cacheSolve(cacheMat)

## makeCacheMatrix: This function creates a special "matrix" object that can 
##                  cache its inverse.
makeCacheMatrix <- function(thisMatrix = matrix()) {
        # A cache of the inverse of the matrix thisMatrix
        # Note that NULL is not the inverse of any matrix
        inverseCache <- NULL
        
        # sets the underlying matrix
        set <- function(newMatrix) {
                thisMatrix <<- newMatrix
                inverseCache <<- NULL # Invalidate cache
        }
        
        # gets the underlying matrix
        get <- function() {
				thisMatrix
        }
        
        # sets the inverse; the caller is responsible for ensuring that
        # the value passed is correct
        setInverse <- function(newInverse) {
				inverseCache <<- newInverse
        }
        
        # gets the inverse from the cache if it exists, or NULL if it does not
        # The caller is responsible for handling the NULL case
        getInverse <- function() {
				inverseCache
        }
        
        # Return a list of the public functions for this object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
##             returned by makeCacheMatrix above. If the inverse has already
##             been calculated (and the matrix has not changed), then
##             cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(cacheMatrix, ...) {
        inverse <- cacheMatrix$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- cacheMatrix$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}