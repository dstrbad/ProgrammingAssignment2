## create special "matrix" object and caches inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
 
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)

}


## computes inverse, otherwise if inverse has been calculated and matrix is not changed
## retrieves inverse from cache

cacheSolve <- function(x, ...) {
    m <- x$getInverse()

    if (!is.null(m)){
        message('getting cached data')
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
