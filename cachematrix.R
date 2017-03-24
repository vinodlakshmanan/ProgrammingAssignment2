# This script provides an implementation of a cache
# which holds the inverse of a given matrix
# makeCacheMatrix <- Object which describes a "cache record"
# cacheSolve <- Functions that builds / queries the cache
# Sample Test case:
# a <- makeCacheMatrix(matrix(1:4 ,2 ,2))
# a$getinv() ... should return NULL
# cacheSolve(a) ... should calculate and return inverse
# cacheSolve(a) ... shoule return cached value
# a$getinv() ... should return the cached inverse

###############################################################
# makeCacheMatrix is the implementation of a "Cache Record"
# It is a self-contained object, which holds both the matrix
# and its inverse. The function returns a list
# containing mutator and accessor (getters and setters) for the
# main matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get
       , setinv = setinv, getinv = getinv)
}


# cacheSolve is the entry point function for the cache
# It takes an object of type "makeCacheMatrix" and
# 1. CHecks if the matrix is square
# 2. If square, Returns the inverse if it exists; if not square, throws up an error
# 3. Calculates, sets and returns the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  input <- x$get()
  inv <- solve(input,...)
  x$setinv(inv)
  inv

}
