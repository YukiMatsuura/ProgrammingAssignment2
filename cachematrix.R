## These functions create a special matrix object, and
## calculate and cache the inverse of the matrix. 
## If the inverse has already been calculated, then 
## the inverse is retrieved from the cache.

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		cache <- NULL
		set <- function(y) {
			x <<- y
			cache <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse) cache <<- inverse
		getInverse <- function () cache
		list(set = set, get = get, 
			setInverse = setInverse, 
			getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
		cache <- x$getInverse()
		if(!is.null(cache)) {
				message("getting cached data")
				return (cache)
		}
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$get()
        cache <- solve(matrix, ...)
        x$setInverse(cache)
        cache
}
