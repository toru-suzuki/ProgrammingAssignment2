## Put comments here that give an overall description of what your
## functions do

## Objective of following function is to solve matrix inversion efficiently.
## This file contains two functions, makecacheMatrix() and cacheSolve()
## First function defines sub-functions and need to be called at least onece.
## After makecacheMatrix() is called, you can call cacheSolve() function 
## as many times you want. But when cacheSolve() is called at fiest time,
## it need full calculation time. But after second times, cacheSolve() function
## returns the result without calculation. It only returns the stored value
## which was calculatied at first time call.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## x: is matrix which is invertible
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
