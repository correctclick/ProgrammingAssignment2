## The functions provided here helps caching an inverse of a matrix
## to avoid expensive computation

## This makeCacheMatrix function creates a matrix object and 
## provides set, get, setInverse and getInverse functions in it.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(invMatrix){
                inv <<- invMatrix
        }
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This cacheSolve function returns an inverse of the special matrix. It  fetches the 
## inverse from cache if present, if not computes one, caches it and returns.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
