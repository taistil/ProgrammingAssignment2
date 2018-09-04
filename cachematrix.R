##This caches the matrix for later use by cacheSolve

makeCacheMatrix <- function (x = matrix())
{
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function solves the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
