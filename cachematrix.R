## These function allow us to cache the inverse of a given matrix, instead of having to calculate it each time.
## The inverse is calcuted (and cached) upon first call for a matrix
## Upon subsequent calls, the cached inverse is returned for this matrix (provided the matrix has not changed)



## This function accepts a standard invertable matrix, and enhances it with an additional list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## This function accepts a matrix as created by the makeCacheMatrix() fuction
## and returns the cached inverse of the matrix if it has previously been calculated,
## or calculates, caches and returns the inverse of the matrix (if it hasn't previously been calculated)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached inverse")
        	return(inv)
        }
        message("calculating inverse")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
    }