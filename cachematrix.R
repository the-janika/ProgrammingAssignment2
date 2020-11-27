## The goal of these functions is to optimise repeated computing of matrix inversion. The functions operate together as the first function provides a list
## of functions to apply and the second function deals with necessary computations, storing the matrix inversion and retrieving the appropriate
## cached inversion.

## The goal of this function is to create a set of functions, which can be used by the caching function. The output of this function
## is a named list, that contains functions to (1) set the matrix and its inverse value, (2) get the matrix from the parent environment, (3) set the
## corrseponding inverse matrix, (4) get the inverse matrix from the parent environment

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_mat <<- solve
  getinv <- function() inv_mat
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The goal of this function is to optimise repeated computing of matrix inversion by caching the result in the memory. If the corresponding matrix inverse
## has already been calculated, it will retrieve the computed inverse from the memory. If the matrix has changed and/or the inverse has not been computed
## the function will retrieve the matrix and compute its inverse. The result is then stored in the memory and can be quickly retrieved.

cacheSolve <- function(x, ...) {
  inv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinv(inv_mat)
  inv_mat
        ## Return a matrix that is the inverse of 'x'
}
