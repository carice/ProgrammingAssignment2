## Matrix inversion is usually a costly computation.
## These functions cache the inverse of a matrix for use later
## without re-computation or explicit definition of a new variable.


## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse to null
  inverse <- NULL
  ## Define set
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## Define get for getting the value
  get <- function() x
  
  ## Define setinv, getinv to get the inverse if it's stored or 
  ## get it, respectively
  setinv <- function(solve) inverse <<- solve
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the stored value in inverse
  inverse <- x$getinv()
  
  ## If there is an inverse there, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## Otherwise, get the values of the matrix, invert it and cache/save the 
  ## inverse, then return the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}

