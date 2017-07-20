## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a list with setters and getters for the 
## matrix and the inverted matrix
## cacheSolve caches the values for the matrix and the inverted 
## matrix using the list returned by makeCacheMatrix
##
## Example:
## mymat <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
## ma <- makeCacheMatrix(mymat)
## cacheSolve(ma)
##
## To check:
## identical(cacheSolve(ma), solve(mymat))

## Write a short comment describing this function
##
## Creates a list with setters and getters for the 
## matrix and the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
    	x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
    	setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
##
## Takes the value of makeCacheMatrix as argument
## Returns the cached data or adds the data to the 
## cache if the cache is empty and returns the data

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
