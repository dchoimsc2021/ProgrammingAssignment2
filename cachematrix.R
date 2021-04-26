makeCacheMatrix <- function(x = matrix()) ##makeCacheMatrix has inv, set, setInverse, getInverse
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}           ##this is the function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}       ##function to obtain inverse of the matrix

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){                  ##checks if the inverse is a NULL
    message("getting cached data") 
    return(inv)         ##reuturns an inversed version
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}