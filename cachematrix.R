## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function establishes a matrix object which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##This function calculates the inverse of the matrix create above
##If it has already been calculated and the matrix has not been changed, it will retrieve the inverse from cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inv(data, ...)
  x$setinverse(inv)
}
