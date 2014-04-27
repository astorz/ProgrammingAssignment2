## The two functions makeCacheMatrix() and cacheSolve() allow the user to 
## calculate the inverse of a matrix and store it in the cache.
## Once computed and stored, it can then be retrieved from the cache
## without needing to be computed a second time.

## This function will calculate the inverse of a matrix and store the value in the cache.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function retrieves the inverse matrix from the cache, if possible.
## If it cannot be found in the cache, the inverse matrix is computed. 
## In either case, the inverse matrix is returned. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
