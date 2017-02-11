## Put comments here that give an overall description of what your
## functions do

## Function to create and to get the matrix, aswell as to create and get the inverse matrix, and finally, also caching it with the list.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) i <<- inverse
        
        getInverse <- function() i
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Tries to get the inversed matrix, if it is null, then it gets the matrix, makes its inversion and caches it, if the inversion already
## exists, it returns it.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        
        if(!is.null(i)) {
            message("getting cached matrix")
            return(i)
        }
        
        matrix <- x$get()
        
        i <- solve(matrix)
        
        x$setInverse(i)
        
        i
}
