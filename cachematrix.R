## Two functions to store a matrix and use the cached inverse

## makeCacheMatrix, creates a matrix with methods to set and 
##                  recall a cached inverse
## Requires: An invertible matrix
## Modifies: x, inv
## Returns: can return the stored matrix and stored 
##          inverse for internal use

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(y) inv <<- y
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}

## cacheSolve, returns the cached inverse if it exists, if not,
##             then it finds the inverse, caches and returns it.  
## Requires: The input to be an invertible matrix created with 
##           makeCacheMatrix
## Modifies: inv
## Returns: inv, the inverted matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("is cached")
    return(inv)
  }
  invert <- x$get()
  inv <- solve(invert, ...)
  x$setInverse(inv)
  inv
}

