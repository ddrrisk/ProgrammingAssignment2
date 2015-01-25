## These functions cache the inverse of a matrix.
## Creates a matrix that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(y) {
    x <<- y
    b <<- NULL
  }
  get <- solve(x)
  setinverse <- function(solve) b <<- solve
  getinverse <- function() b
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Computes the inverse of makeCacheMatrix.
cachesolve <- function(x, ...) {
  b <- x$getinverse()
  if(!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  data <- x$get()
  b <- solve(data, ...)
  x$setinverse (b)
  b
}
