## The pair of functions described above cache the inverse of a 
## matrix.

## Following the same logic of the example given in the assignment,
## the function "makeCacheMatrix" creates a list containing a 
## function to set and get the value of the matrix and its inverse,
## respectively.
## This function has only one argument (x), which -by default- is a 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  imx <- NULL
  set <- function(y) {
    x <<- y
    imx <<- NULL
  }
  get <- function() x
  setinv <- function(solve) imx <<- solve
  getinv <- function() imx
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function "cacheSolve" calculates the inverse of the matrix
## returned by the above function. In case the inverse has already
## been calculated (and the matrix has not changed), then
## "cacheSolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    imx <- x$getinv()
    if(!is.null(imx)) {
      message("getting cached data")
      return(imx)
    }
    data <- x$get()
    imx <- solve(data, ...)
    x$setinv(imx)
    imx
}
