## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize inverse as NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse when matrix is updated
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getInverse <- function() inv
    
    # Return a list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # If inverse is already cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)  # Cache the inverse
    inv
}