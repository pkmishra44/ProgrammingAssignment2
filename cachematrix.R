## writing a pair of functions that cache the inverse of a matrix

## writing makeCacheMatrix function to create a list containing a function to 
#1 set and get the value the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y;
        invrs <<- NULL;
    }
    get <- function() x;
    setinverse <- function(inverse) invrs <<- inverse;
    getinverse <- function() invrs;
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## writing a functin to compute the inverse of the special "matrix" returned by makeCacheMatrix. 
## the function cacheSolve(below) will first check if the inverse has been computed and will retrieve the inverse from cache
## Otherwise it will compute the inverse and set the value in cache using setinverse function


cacheSolve <- function(x, ...) {
      invrs <- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data.")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data)
    x$setinverse(invrs)
    invrs
}
