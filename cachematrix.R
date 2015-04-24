## The cachematrix.R file contains 2 functions: makeCacheMatrix and cacheSolve
##
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
##
## cacheSolve computes the inverse of the special "matrix" object returned by the
## makeCacheMatrix and returns the inverse

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## This special "matrix" object has functions to 
## 1) get/set the value of the input matrix
## 2) get/set the value of the matrix inverse stored in cacahe 

makeCacheMatrix <- function(x = matrix()) {

  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setmatInv <- function(matInv) invM <<- matInv
  getmatInv <- function() invM
  list(set = set, get = get,
       setmatInv = setmatInv,
       getmatInv = getmatInv)
}


## cacheSolve computes the inverse of the special "matrix" object returned by the
## makeCacheMatrix and returns the inverse
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
## This functions uses solve() function to compute inverse, input matrix is 
## assumed to be square.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  invM <- x$getmatInv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setmatInv(invM)
  invM
}
