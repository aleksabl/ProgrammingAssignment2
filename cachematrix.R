## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix which can be used to cache the inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    #When setting a new matrix, the inverse needs to be cleared as well
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, 
       get=get, 
       getInverse=getInverse, 
       setInverse=setInverse)
}

## Returns a matrix that is the inverse of x. The result is cached so subsequent calls to this function
## returns the same result without computing it again.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(is.null(i)) {
    #We need to do the computation
    i <- solve(x$get())
    x$setInverse(i)
  }
  
  i
}
