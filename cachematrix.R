## Functions for caching matrix operations
## like the inverse in order to optimize repeated access patterns


## create an "object" containing a matrix and its cached inverse

makeCacheMatrix <- function(matrix = matrix()) {
   inverse <- NULL
   set <- function(mat) {
        matrix <<- mat
        inverse <<- NULL
   }
   get <- function() matrix
   setinverse <- function(inv) inverse <<- inv
   getinverse <- function() inverse
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Get the - possibly cached - matrix inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(is.null(inv)) {
                message("computing and caching inverse")
                mat <- x$get()
                inv <- solve(mat)
                x$setinverse(inv)
        }
        inv
}
