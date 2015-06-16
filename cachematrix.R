## The code in this script is used to compute the inverse of a matrix.
## To make this operation less costly caching is used to retrieve a known
## value when this is available.
##
## Use:
##   x <- makeCacheMatrix(matrix(c(2,2,3,4,5,9,7,8,9), nrow = 3, ncol = 3))
##   cacheSolve(x)
##   ## The next operation retrieves from cache 
##   cacheSolve(x)
##   x$set(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
##   ## The next operation is on a new matrix so cannot be done from cache
##   cacheSolve(x)


## Create a list which knows how to set/get the value of the matrix and
## set/get the value of the inverse. This is used to hold the cached values.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of a matrix, if possible use a cached value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
