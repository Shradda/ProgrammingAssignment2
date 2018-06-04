## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(p = matrix()) {
  inv <- NULL
  set <- function(y) {
    p <<- y
    inv <<- NULL
  }
  get <- function() p
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}	 

cacheSolve <- function(p, ...) {
  ## Return a matrix that is the inverse of 'p'	         ## Return a matrix that is the inverse of 'p'
  inv <- p$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- p$get()
  inv <- solve(data, ...)
  p$setinv(inv)
  inv
}	 



