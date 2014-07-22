## Functions to implement a special type of matrix that can cache its inverse,
## reducing the cost of computing it repeatedly every time it is needed.


## Creates a special "matrix" object that can cache its inverse to improve performance.
## The function returns a list containing functions to
##    1. set the value of the matrix (set)
##    2. get the value of the matrix (get)
##    3. set the value of the inverse (setinverse)
##    4. get the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() {inverse}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## the function retrieves the inverse from the cache instead of computing it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (is.null(inv)) {
        ## Inverse not available, computation is needed
        message("Computing the inverse of the matrix")
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
    }
    else {
        ## Cached data, no computation needed
        message("Getting cached data")
    }
    ## Finally, return the computed or cached data
    inv
}
