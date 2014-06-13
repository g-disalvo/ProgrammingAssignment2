## This provides a set of functions to create and manage a cacheMatrix object
## They streamline the evaluation of a matrix inverse by performing the 
## calculation only if it hasn't been computed previously. When initially 
## computed for a given matrix, the inverse is cached within the object 
## allowing rapid return for future calls.

## Sample use:
# > m <- makeCacheMatrix()
# > m$set(matrix(1:4, 2, 2))
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m$getinv()
# NULL
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## GD - 6/13/14 - initial release

makeCacheMatrix <- function(x = matrix()) {
## create a cacheMatrix object
## methods include: set(matrix), get(), setinv(inverse), getinv(), list()
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y  # set new matrix
        inv <<- NULL  # inverse is not available for new matrix assignment
    }
    
    get <- function() {
        x  # return current matrix
    }
    
    setinv <- function(solve) inv <<- solve
    
    getinv <- function() {
        inv  # return inverse
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    ## if the inverse has already been computed and cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## otherwise calculate it
    data <- x$get()
    inv <- solve(data, ...)
    ## store in cacheMatrix object
    x$setinv(inv)
    inv
}
