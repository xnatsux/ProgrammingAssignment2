## This file contains two functions, makeCacheMatrix and cacheSolve
## these functions are desigend to cache a matrix that is a function input to makeCacheMatrix
## and to invert the matrix if it has not been previously inverted

## First is the makeCacheMatrix function, which creates a special output that can be fed to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Second is the cacheSolve function, which checks if the function has been inverted and 
## calls the cached version if it has been previously inverted (or inverts otherwise)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
