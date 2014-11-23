## @author: brennebeck
## @note: no error checking per assumption of square matrices

## object (i.e. class in other langs) to get/set the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(x) inv <<- solve(x)
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## memoization in action :)
## runs an initial computation (inverse of matrix)
##   and returns the value while caching it
## when func is re-run, if cache exists, skip computation!
cacheSolve <- function(x, ...) {
  # set the cache, null or otherwise
  inv <- x$getInverse()
  if(!is.null(inv)) {
    #our cache is not null, return cache
    message("cache exists, returning cache")
    return(inv)
  }
  #we have no cache!!! do all the things!@
  inv <- x$setInverse(x$get())
  inv
}
