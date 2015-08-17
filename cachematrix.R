## The functions cache the computation of the inverse of a matrix.

## Creates a special object that stores a matrix.

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Takes the stored matrix and returns its inverse. The value is computed only
## if it is not cached, i.e. if it has not been computed before.

cacheSolve <- function(x, ...) {

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
