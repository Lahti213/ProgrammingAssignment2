## This set of functions will cache the inverse of a matrix that the 
## user may need to use again rather than recomputing the inverse.

## This function takes the input of a matrix and creates a "special" 
## matrix. The "special" matrix is simply a list containing four functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse of the "special" vector created with
## the above functions. However, first it checks if the inverse has already
## been calculated. If it has, it pulls the mean from cache, skipping the
## computation. Otherwise, it calculates the inverse and sets the value of
## the inverse in the cache by way of the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
