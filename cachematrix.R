
## makeCacheMatrix creates a special matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x,...) {
        
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setInverse(i)
  i
}
