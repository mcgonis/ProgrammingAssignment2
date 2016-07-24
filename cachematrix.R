## These functions are for the caching of an inverse matrix
## The cached matrix itself is stored in the 'matrix' object, 
## while the logic to check if the cached value is present is in the 
## cacheSolve function.

## Create an object which contains the vector, possibly the cached inverse and getters/setters for each. 
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
           x <<- y
           m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Get the cached inverse or compute it if not present.
cacheSolve <- function(x, ...) {
   m <- x$getInverse()
   if(!is.null(m)) {
       message("getting cached data")
       return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setInverse(m)
   m
}
