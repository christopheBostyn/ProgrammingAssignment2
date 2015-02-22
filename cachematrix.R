
## With below (nested) functions it is feasible to set and/or get matrices and their inverses
## to avoid calculating the inverse multiple times, we first look into the cached memory to find the solution.


## This function will return a list of 4 functions to get/set a matrix or its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseM) inverseMatrix <<- inverseM
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Below function will return a matrix that is the inverse of 'x'
## if the inverse is already calculated, then the cached solution will be taken instead of recalculating the inverse.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  else {
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    return(i)
  }
}
