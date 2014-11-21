## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" objects with setters and getters

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Returns the cached inverse matrix of the matrix provided as argument if the cache exists, or calculates the inverse, caches it and then returns the inverse matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
