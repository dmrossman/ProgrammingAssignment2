## Creates an object that caches the inverse matrix since this is a time 
##   expensive operation

## makeCacheMatrix creates an object that represents a matrix but caches
##   the inverse matrix so that it does not need to be recomputed each 
##   time.  Creates an internal 

makeCacheMatrix <- function(internalMatrix = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        internalMatrix <<- y
        invMatrix <- NULL
    }
    
    get <- function() internalMatrix
    setinv <- function(inv) invMatrix <<- inv
    getinv <- function() invMatrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## CahceSolve returns a cached inverse matrix if it has already been calculated
##  If not, it calculates the inverse, stores it for future and returns the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinv()
    if(!is.null(invMatrix)) {
        message("getting cached inverse matrix")
        return(invMatrix)
    }
    
    # We didn't have a cached inverse, so create it
    internalMatrix <- x$get()
    invMatrix <- solve(internalMatrix)
    x$setinv(invMatrix)
    invMatrix
}