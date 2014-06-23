## These functions are a part of Programming Assignment 2 for the R Programming class.
## These functions are an exercise in caching in R and in particular take a matrix
## and first cache it and then on subesquent calls retrieve that matrix to avoid the computational
## costs.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv_m <<- inverse
        
        getinverse <- function() inv_m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        ## where the magic happens...really the only difference between our mean axample and this one
        inverse <- solve(data, ...)
        
        x$setinverse(m)
        m
}
