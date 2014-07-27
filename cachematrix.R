## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## gets the value of the matrix
  get <- function() x
  ## sets the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  ## gets the value of the inverse
  getinverse <- function() m
  ##lists the functions inside makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix. If the inverse has already been 
##  calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ##  checks if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    ## returns the previously calculated value
    return(m)
  }
  data <- x$get()
  ## Return a matrix that is the inverse of 'x'
  m <- solve(data, ...)
  ## sets the the calculated value
  x$setinverse(m)
  ## returns the calculated value
  m
}