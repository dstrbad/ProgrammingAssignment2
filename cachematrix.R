## creates special "matrix" object that can cache its inverse

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


## computes inverse of special "matrix" object
## if inverse has been calculated and matrix is not changed
## retrieves inverse from cache otherwise inverse is calculated

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
