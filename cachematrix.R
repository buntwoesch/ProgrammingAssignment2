## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ##set inv to NULL for later use
  inv <- NULL
  
  ## function sets matrix x to matrix y, again setting inv to NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##returning matrix x
  get <- function() x
  
  ## setting inv to inverse
  setInverse <- function(inverse) inv <<- inverse
  
  ##returning inv
  getInverse <- function() inv
  
  ##returning the above functions 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## returning inverse matrix inv of matrix x
  inv <- x$getInverse()
  
  ##in case inv exists, cache variable is evaluated 
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
