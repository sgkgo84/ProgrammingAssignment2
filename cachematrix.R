## Because generating an inverse matrix can be a heavy user of computer
## resouces, when an inversion will be used on a regular basis it is more
## efficient to store the inversion and use that for calculations.

## This function sets up the environment for caching an inverson matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_matrix <<- solve
        getinverse <- function() inv_matrix
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will provide an inversion martrix for use in calculations.
## If the matrix already exists, the matrix will be retrieved, otherwise the
## inversion will be calculated and cached.


cacheSolve <- function(x, ...) {
        inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix)) {
                message("Retrieving cached data")
                return(inv_matrix)
        }
        datamatrix <- x$get()
        inv_matrix <- solve(datamatrix, ...)
        x$setinverse(inv_matrix)
        inv_matrix
}
