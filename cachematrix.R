## This function creates a matrix, that it's really a list
makeCacheMatrix <- function(x = matrix()) {
## Set the value of the matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
## Get the value of the matrix       
        get <- function() x
## Set the value of the inverse        
        setinverse <- function(inverse) inv <<- inverse
## Get the value of the inverse        
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## This function returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
## If the inverse has already been calculated the function retrieves the inverse
## from the cache.
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
## If not, the function gets it from the input matrix        
        data <- x$get()
## Computes the inverse of it        
        inv <- solve(data, ...)
## It sets the inverse         
        x$setinverse(inv)
## Returns the value of it        
        inv
}