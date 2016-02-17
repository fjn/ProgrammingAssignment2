## The following two functions create a a special object that stores a matrix
## and caches its inverse.
##
## Usage example:
##
## > x <- rbind(c(2,4),c(6,9))
## > xm <- makeCacheMatrix(x)
## > xmInv <- cacheSolve(xm)
## > xm %*% xmInv  # the Identity matrix
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

makeCacheMatrix <- function(x = matrix()) {
    # creates a list of functions to handle matrix inversion
    #
    # Args:
    #   x: a matrix. Default value is an empty matrix
    #
    # Returns:
    #   a list of functions:
    #       set: set the value of the matrix
    #       get: get the value of the matrix
    #       setInverse: set the value of the matrix inverse
    #       getInverse: get the value of the matrix inverse

    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv

    # return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    # Calculates the matrix inverse for the matrix created
    # with makeCacheMatrix. Before calculating the inverse using the 'solve'
    # function, check if inverse has already been calculated. If so return the
    # cached version.
    #
    # Args:
    #   x: the list of functions returned by makeCacheMatrix
    #   ... : any other arguements to pass to the 'solve' function
    #
    # Return a matrix that is the inverse of the matrix stored in 'x'

    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached matrix data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
