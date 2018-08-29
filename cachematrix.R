## These functions allow for the caching of the inverse of a matrix to that computation time and resources can be 
## saved when the inverse of the same matrix needs to be calculated repeatedly.

## This function creates a 'special' matrix object that allows for the matrix inverse to be cached.

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


## This function returns the inverse of the matrix from cache if it has already been calculated or calculates and
## stores the inverse in cache if it has not. It accepts a 'special' matrix object as created by the above
## function as input

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
        ## Return a matrix that is the inverse of 'x'
}
