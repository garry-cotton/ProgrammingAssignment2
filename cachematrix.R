## Allows for caching of matrix inversion.

## Creates a special cached matrix object for use in matrix inversion.
makeCacheMatrix <- function(x = matrix())
{
    inverse <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}

## Inverts cache matrix object by providing cached value where applicable,
## else computes inverted matrix
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
