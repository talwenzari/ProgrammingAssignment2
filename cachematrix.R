## The following functions form the solution to the programming assignment 2
## of the Coursera course R Programming.
##
## The functions demonstrate the way in which lexical scoping can be used to 
## encapsulate data and functions in R, in this case to cache data of potentially
## expensive calculations.


## This function creates a "matrix object" which provides methods for
## caching the inverse of the given matrix x

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse to null
  inverse <- NULL
  
  # method that resets the cached matrix
  set <- function(y) {
    # reset matrix x
    x <<- y
    
    # also reset the inverse
    inverse <<- NULL
  }
  
  # method that returns the cached matrix
  get <- function() x
  
  # method that caches the inverse of the matrix 
  setInverse <- function(inv) inverse <<- inv
  
  # returns the cached inverse matrix
  getInverse <- function() inverse
  
  # create and return the matrix "object" (i.e. list of named functions)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse matrix of the matrix object x given as
## argument. The matrix object x should be initialized with the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (! is.null(i)) {
    # the inverse has already been calculated, return the cached value
    return(i);
  }
  # We need to calculate the inverse with solve
  inv <- solve(x$get())
  
  # cache the inverse for future reference
  x$setInverse(inv)
  
  # return the inverse value
  inv
}
