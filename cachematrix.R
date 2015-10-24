

## This function creates an object which is a list of a matrix, a way to set this 
## matrix to a different value, a way to calculate and set the inverse and then
## cache the value, and a way to retreive the cached inverse value

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function() m <<- solve(x)
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function takes a matrix object as created by makeCacheMatrix and gets
## its inverse. It will first check to see if the inverse is cached, and if so
## it will retrieved that cached value. If it is not then it will use the 
## setInverse part of the object to find the inverse and cache it for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      m <- x$setInverse()
      m
}
