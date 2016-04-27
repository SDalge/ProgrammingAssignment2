## the 'makeCacheMatrix function creates a 'special' matrix that can cache its inverse
## the 'cacheSolve function compute de inverse of the matrix or return the cached value if
##   it has been already calculated.




## 'makeCacheMatrix' basically does the following:
## 1 - Set the value of the Matrix
## 2 - Get the value of the Matrix
## 3 - Set the value of the inverse matrix
## 4 - Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  u <- NULL
  set_i <- function(p){
    x <<- p
    u <<- NULL
  }
  get_i <- function() x
  setinv <- function(solve) u <<- solve
  getinv <- function() u
  list(set_i = set_i, get_i = get_i, setinv = setinv, getinv = getinv)
  
}


## 'cacheSolve' function computes the inverse of the 'special' matrix returned by 
##   'makeCacheMatrix', or retrieve the inverse from the cache if it has already been
##    calculated.

cacheSolve <- function(x, ...) {
  u <- x$getinv()
  if(!is.null(u)) {
    message("getting cached data")
    return(u)
  }
  data_i <- x$get_i()
  u <- solve(data_i, ...)
  x$setinv(u)
  u
  
}
