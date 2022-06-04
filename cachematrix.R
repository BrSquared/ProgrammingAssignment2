## 2 functions to create a cachable matrix and get the inverse

## makeCacheMatrix creates the cache for the matrix supplied

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    
}


## CacheSolve checks the cache for the inverse and calculates
## it if it is not already there

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        print("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
