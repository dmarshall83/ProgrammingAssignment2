## These functions create a special matrix that can cache its values 
## and the inverse of its values.

## makeCacheMatrix created the special matrix
## set stores the matrix value in a cached variable.
## get retrives the stored matrix.
## setinverse stores the inverse of the matrix in a cached variable.
## getinverse retrives the stored inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(val) i <<- val
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## First the special matrix is checked to see if there is a cached inverse.
## If a cached inverse in found it is returned and the function stops.
## If a cached invers is no found it calculates the inverse and sets in on
## the special matrix.

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
