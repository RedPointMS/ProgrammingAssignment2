## Put comments here that give an overall description of what your
## functions do

## This function caches inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize result
        invMatrix <- NULL
        
        ## set matrix
        set <- function (mx){
                x <<- mx
                invMatrix <- NULL
        }
        
        ## get original matrix
        get<- function() x
        
        ## set inverted matrix
        setInverted <- function(solve) invMatrix <- solve
        
        ## get inverted matrix
        getInverted <- function() invMatrix
        
        ## make functions available as part of makeCacheMatrix
        list(set = set, get = get, invertMatrix = setInverted, getInvertedMatrix = getInverted)
        
}


## This function computes inverse of the matrix. If such inversion exists already, return cached value
## otherwise force the calculation and store in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check if inverted matrix exists, if so return its value
        invMatrix <- x$getInverted()
        if (!is.null(invMatrix)){
                message("getting inverted matrix")
                return (invMatrix)
        }
        
        ## if inverted matrix doesn't exist yet, invert it and set cache to new value
        ## solve function is called with only one parameter forcing use of identity matrix
        data <- x$get()
        invMatrix <- solve(data)
        x$setInverted(invMatrix)
        
        # return newly computed value
        invMatrix
                
        
}
