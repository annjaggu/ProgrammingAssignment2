## With following two functions, we are trying to cache inversion of matix.
## Cached inversion will be used if its avaialble hence computation will be faster.

## This function is a special matrix which caches the inversion of provided matrix

makeCacheMatrix <- function(m = matrix()) {
  invs <- NULL
  set <- function(m1) {
    m <<- m1
    invs <<- NULL
  }
  get <- function() m
  setInversion <- function(solve) invs <<- solve
  getInversion <- function() invs
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## This function returns a matrix which is inversion of provided special matrix 'makeCacheMatrix'.
## It checks if inversion is cached then return the same else compute the inversion.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getInversion()
  if(!is.null(invs)) {
    messge("Getting cached inversion")
    return(invs)
  }
  m <- x$get()
  invs <- solve(m, ...)
  x$setInversion(invs)
  invs
}
