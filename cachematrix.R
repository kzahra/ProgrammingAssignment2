## this function is used to create a custom object
## which has a matrix and its inverse. When this constructor
## is used a 'cachable matrix', these are created in the R environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## this function checks whether the inverse of 
## the matrix is available from cache. If so, it
## returns the inverse matrix. If not, then
## the inverse of the matrix is calculated and stored in 
## the cache for next time and returned.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}