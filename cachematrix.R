## This R code gives us a pair of functions to create a special matrix object and
## allow us to store a cached copy of its inverse to minimise the computational cost
## of inverseing a matrix multiple times.

## The first function (makeCacheMatrix )creates a special matrix object. The object
## is a list which contains functions to get/set the matrix and its inverse.
## the matrix is stored in the scope of this function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function is used to return the inverse of the matrix.
## it attempts to 'get' the inverse, if it is not null, it returns the inverse.
## if it is null, it calculates it, 'sets' it and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
