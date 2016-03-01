## These functions provide the ability to create the inverse of a matrix and cache it so that it can be retrieved and reused each time without recalculation 

## This function implements a cache to store and retrieve a matrix and its inverse. It returns a list of functions, set(), get(), setinverse(), getinverse() that can be performed on the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of a matrix created by makeCacheMatrix. It first checks to see if the inverse exits. If so, it gets the inverse from the cache and skips creating an inverse matrix. Otherwise it creates the inverse and stores it via the setinverse() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        return(inv)

}
