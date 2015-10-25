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

## Test Cases

## Create a simple Matrix
mat <- matrix(1:4,2,2)
mat
## Call makeCacheMatrix to get the "Special matrix
matrixSp <- makeCacheMatrix(mat)

## Call cacheSolve to get the inverse first time
inverseMatrix <- cacheSolve(matrixSp)
inverseMatrix

## Test by multiplying simple matrix & it's inverse to ensure it returns an identity matrix
mat %*% inverseMatrix

## Now call cacheSolve again to test whether it returns from cache
inverseMatrix <- cacheSolve(matrixSp)
inverseMatrix

## Again test by multiplying simple matrix & it's inverse to ensure it returns a unit matrix
mat %*% inverseMatrix

## Now change the simple matrix & repeat the test
mat <-  matrix(11:14,2,2)
mat
matrixSp <- makeCacheMatrix(mat)
inverseMatrix <- cacheSolve(matrixSp) ## It should NOT return from cache
inverseMatrix

## Now call again and it should return from cache
inverseMatrix <- cacheSolve(matrixSp) 
inverseMatrix

