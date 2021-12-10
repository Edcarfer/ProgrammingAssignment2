## Couple of functions that create a special "Matrix" object, which is a list
## containing functions for setting and getting the matrix, and its inverse
## and another function which obtains the inverse if not cached already.
## This is an assignment of Coursera Course, so please follow the Honor Code.
### As a possible improvement, the union of both functions in a special object
### could be interesting.

## Creation of a special "Matrix" object.

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


## Solve matrix function, which retrieves the cached value if it exists

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
