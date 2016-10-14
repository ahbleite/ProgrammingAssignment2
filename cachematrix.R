## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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


#source("cachematrix.R")
#my_matrix <- makeCacheMatrix( matrix(c(4,3,3,2),2,2))
#cacheSolve(my_matrix)
