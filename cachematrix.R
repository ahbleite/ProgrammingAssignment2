##  These functions create an special matrix that can calculate and cache its own inverse 

## Function Name:
## Description: Creates a special "matrix" object that can cache its own inverse

##  Function:
##    makeCacheMatrix <- function(x = matrix())
##  Args:
##    x: an invertible matrix
##  Description: Returns the inverse of a matrix from the cache, 
##    Creates a special "matrix" object that can cache its own inverse
##  Returns:
##    The inverse of matrix x
##  Sample: 
##    source("cachematrix.R")
##    my_matrix <- makeCacheMatrix( matrix(c(4,3,3,2),2,2))
##    cacheSolve(my_matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setinversion <- function(inversion) i<<-inversion
  getinversion <- function() i
  list(set = set, get = get,
       setinversion = setinversion, getinversion = getinversion)
}


##  Function:
##    cacheSolve <- function(x, ...)
##  Args:
##    x: an "special" matrix
##  Description: 
##    Returns the inverse of the  matrix from the cache, 
##    if the cache is null calculates the inverse, stores in the cache and
##    then returns it
##  Returns:
##    The inverse of x
##  Sample: 
##    source("cachematrix.R")
##    my_matrix <- makeCacheMatrix( matrix(c(4,3,3,2),2,2))
##    cacheSolve(my_matrix)
cacheSolve <- function(x, ...) {
  i <- x$getinversion()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  i
}