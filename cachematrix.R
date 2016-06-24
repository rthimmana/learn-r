## The special matrix with the associated functions is created by passing 
## the martix to this function  
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        # Stores the matrix in the enclosing environment
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


## when retriving the inverse, we first look for a cached inverse
## if present we return that cached value
## else calculate, stored the cached value in the enclosing environment
## and use it !
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    data <- x$get()
    if(!is.null(i)) {
        ## computed and cached !
        message("getting cached data")
        return(i)
    }
    # not computed yet, lets compute and cash the inverse, now 
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
