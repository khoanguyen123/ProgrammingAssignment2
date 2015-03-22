## These two functions together save calculation cost and reduce response time
## by caching the inverse of a matrix. The cached copy will be returned if found, 
## otherwise the calculation is performed and the result is cached for future reuse

## makeCacheMatrix "decorates" a matrix 
## with a list of functions that support get/set of matrix
## and get/set of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
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

## cacheSolve takes a decorated matrix produced
## by makeCacheMatrix and returns the inverse
## of that matrix. If the inverse is null, 
## this function computes the inverse, saves
## it in the cache, and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if (!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        } 
        ## Cache miss. Calculate inverse and update cache
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        
        ## return inverse
        inverse
}
