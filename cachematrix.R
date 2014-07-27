## makeCacheMatrix and cacheSolve are two functions that are able to cache
## the inverse of a matrix. Caching the inverse of a matrix is useful when
## involving a very large matrix whose inverse needs to be calculated and
## said inverse matrix needs to be retrieved later on.
## Instead of repeatedly computing the inverse of that matrix, the cached
## inverse matrix can be retrieved, thus cutting down on computation and
## using fewer resources of both time and computer power.

## makeCacheMatrix creates a special matrix by making a list that contains
## a function that sets and gets the value of the matrix and sets and gets
## the value of the inverse of the matrix

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



## cacheSolve calculates the inverse of the special matrix that is stored using
## makeCacheMatrix. It first checks the value of the variable "i", which is set
## to "NULL" if no inverse has been calculated. If "i", which is the value of
## the inverse, is not "NULL", it does not calculate the inverse and simply
## retrieves the value from the cache. If it is set to "NULL", it proceeds by
## calculating the value of the inverse of the matrix that it is fed.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
