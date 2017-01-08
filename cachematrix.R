##These two functions are for Programming Assignment 3
## Week 3 of R-Programming on Coursera
## They demonstrate caching of the inverse of a
## matrix.


## The function makeCacheMatrix is basically a 
## list of functions
## that can be used to set values to a matrix, 
## get values from a matrix,
## set the inverse of a matrix to cache
## and get the inverse of a matrix (from cache)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function would be called instead of 
## the solve() function to compute inverse
## This is a smarter way to compute inverse
## as if you have already computed the 
## inverse of the given matrix in the past
## (in a prev. iteration of a loop for example)
## this function just returns the cached value
## cutting down runtime.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
