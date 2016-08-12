## caching the inverse of a matrix 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setV <- function(inv) v <<- inv
    getV <- function() v
    list(set = set, get = get,
         setV = setV,
         getV = getV)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    v <- x$getV()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...) %*% data
    x$setV(v)
    v
}