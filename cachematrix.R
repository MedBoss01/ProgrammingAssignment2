## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- new.env()
  
  # Define methods to set and get the matrix
  m$set <- function(y) {
    x <<- y
    m$inv <- NULL
  }
  m$get <- function() x
  
  # Define methods to set and get the inverse of the matrix
  m$setInverse <- function(solve) {
    m$inv <<- solve
  }
  m$getInverse <- function() {
    # If the inverse has not been calculated, calculate it
    if (is.null(m$inv)) {
      m$inv <<- solve(x)
    }
    m$inv
  }
  
  # Return the list of methods
  m
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$get()
  
  # If the inverse has not been calculated, calculate it
  if (is.null(m$inv)) {
    m$inv <- solve(m)
    x$setInverse(m$inv)
  }
  
  # Return the inverse of the matrix
  m$inv

}
