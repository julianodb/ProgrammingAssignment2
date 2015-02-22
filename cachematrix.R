## Theese functions provide a way to get the inverse of a matrix from a 
## cached value when it has already been calculated before.

## The makeCacheMatrix(x) function creates a list with getters and setters
## function to access the provided matrix (x) and its inverse. When
## setting a new matrix, its inverse is set to NULL.

makeCacheMatrix <- function(x = matrix()) {
        ## Creates an object that can provide the cached value of 
        ## a matrix's inverse
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


## The cacheSolve(x) returns the inverse of the matrix stored in x (where
## x is an object created by makeCacheMatrix). This function verifies if
## the value stored as the inverse (x$getinverse()) is not NULL, in which 
## case it just returns the cached value x$getinverse(). If, on the other
## hand, x$getinverse() is NULL, than the inverse is calculated with
## solve(), stored with x$setinverse() and returned to the user. When the 
## returned value comes from the cache, the message 'getting cached data'
## is displayed.

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

## Example use:
## x <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(x) ## First time the inverse is calculated
## cacheSolve(x) ## Cached value ir provided
## x$set(matrix(rnorm(9),3,3)) ## set new matrix
## cacheSolve(x) ## First time the inverse is calculated
## cacheSolve(x) ## Cached value ir provided
