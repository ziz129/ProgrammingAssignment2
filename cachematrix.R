## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.

## this funtion is used to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        invma <- NULL
        set <- function(y) {
                x <<- y
                invma <<- NULL
                
        }
        get <- function() x
        setinverse <- function(inverse) invma <<- inverse
        getinverse <- function() invma
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## After getting the matrix resulted from makeCacheMatrix,
## this function is used to compute its inverse. If there is no
## no change of the matrix, it just retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invma <- x$getinverse()
        if (!is.null(invma)) {
                message("now retrieving cached data")
                return(invma)
        }
        data <- x$get()
        invma <- solve(data, ...)
        x$setinverse(invma)
        invma
}