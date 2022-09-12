## The functions below allow for caching the value of the inverse of a matrix.
## It's useful when the inverse of a single matrix has to be calculated
## repeatedly. To this end, given a matrix 'x', one creates an object
## 'makeCacheMatrix(x)' and obtains the inverse by calling
## 'cacheSolve(<the_created_object>)'.

## 'makeCacheMatrix' creates an object that stores a matrix 'x' together with
## its inverse. It returns a vector of functions for getting and setting
## the values of both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(new_x) {
        x <<- new_x
        inverse <<- NULL
    }
    getinverse <- function() inverse
    setinverse <- function(new_inverse) inverse <<- new_inverse
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## 'cacheSolve' returns the inverse of the matrix stored in x.
## It uses a cached value of the inverse if available, otherwise the inverse is
## calculated and cached in 'x'.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (is.null(inverse)) {
        inverse <- solve(x$get())
        x$setinverse(inverse)
    } else {
        message("Getting cached value for the matrix inverse!")
    }
    inverse
}
