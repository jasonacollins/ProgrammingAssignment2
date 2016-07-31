# The following two functions are used to compute the inverse of an invertible matrix. The second
# function caches the value of the calculated inverse, saving computational power if the inverse
# has already been calculated.

# This first function creates a list containing a function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

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

# This second function computes the inverse of the "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
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
