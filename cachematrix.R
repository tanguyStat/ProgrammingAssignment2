##This set of two functions can inverse a matrix and cache potentially time 
##consuming computations

## makeCacheMatrix
## The first function defines the matrix, gets its value and do the same thing
## with its inverse. 

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


##cacheSolve
## The following function calculate the inverse of the matrix only if it has not been calculated before.
## If it has been done, it simply retrieves the matrix inverse from the cache.

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

        ## Return a matrix that is the inverse of 'x'
}
