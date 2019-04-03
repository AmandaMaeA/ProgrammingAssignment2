## These functions cache the inverse of a matrix 
## Because matrix computation is a costly function, these functions store the inverse in the cache. 

## This function creates a special matrix object that can cache it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    lists(set = set, 
          get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
}


## This computes the inverse of the special matrix above-
## if the inverse has already been computed, then it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <-solve(mat, ...)
        x$setInverse(inv)
        inv
}

