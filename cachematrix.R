## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather
## than computing it repeatedly.

## makeCacheMatrix: creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	ix <- NULL
	set <- function(y) {
		x <<- y
		ix <<- NULL
	}
	get <- function() x
	setinvx <- function(invx) ix <<- invx
	getinvx <- function() ix
	list(set = set, get = get,
	     setinvx = setinvx,
	     getinvx = getinvx)
}


## cacheSolve: computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ix <- x$getinvx()
	if(!is.null(ix)) {
		message("getting cached data")
		return(ix)
	}
	data <- x$get()
	ix <- solve(data, ...)
	x$setinvx(ix)
	ix
}
