## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Following function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs<<- NULL
  }
  get <- function() x
  setinvrs <- function(X) invrs <<- solve(x)
  getinvrs <- function() invrs
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}

## This function computes the inverse of the matrix created by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <<- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <<- solve(mat)
  x$setinvrs(invrs)
  invrs
}
