## The function makeCache Matrix creates a matrix object that can cache its inverse.
## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, it is retrieved from the cache.

## The function makeCache Matrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x			
	setsolve <- function(s) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, it is retrieved from the cache.

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()	
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
