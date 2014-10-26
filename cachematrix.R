## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. The 2 functions below, makes the cache on the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_x <<- inverse
    getinverse <- function() inverse_x
    list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix calculated from the previous 
## function if the cached inverse exists or if it doesnt it should calculate,
## cache and give the result.
cacheSolve <- function(x, ...) {
    inverse_x <- x$getinverse()
    if(!is.null(inverse_x)) {
        message("getting cached inverse matrix")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(data)
    x$setinverse(inverse_x)
    inverse_x
}				
