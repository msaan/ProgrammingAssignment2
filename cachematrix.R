## Caching the inverse of a matrix. I created tweo functions that are used to create a special object that stores a matrix and caches its inverse. 

## The function makeCacheMatrix creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## The function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse is calculated, then the cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
