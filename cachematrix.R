## ##R Programming - Programming Assignment 2: Lexical Scoping
## 
## #Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. In order to achieve this target, we create two functions: 
##   -makeCacheMatrix, which transform a matrix in a object able to store the 
##    original matrix and the inverted one (if already computed);
##   -cacheSolve, which takes an object produced by the above function and 
##    computes the inverse matrix (if it hasn't been already calculated) or 
##    simply retrieve it from the cache (contained inside the special object). 



## #Special "matrix" object
##
## makeCacheMatrix <- function(x = matrix())
##
## #This function creates a special "matrix" object that can cache its 
## inverse.
##
## Input:  x = a square numeric or complex matrix (supposed to be invertible).
##         Logical matrices are coerced to numeric.
## Output: makeCacheMatrix = a list of subroutines (functions to perform a 
##         specific task), these are: set the original matrix, get it, set the 
##         inverse, get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv.matrix) inv <<- inv.matrix
  getinv <- function() inv
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
  
}



## #Inverse of the special "matrix" (built by makeCacheMatrix)
##
## cacheSolve <- function(x, ...) 
##
## #This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieve the inverse from the 
## cache.
##
## Input:  x = special "matrix" object, created by a call to function 
##         makeCacheMatrix on a usual matrix.
## Output: cacheSolve = the inverse of the matrix defined by the object x.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m

}
