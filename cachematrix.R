## Functions are defined using the function() directive and are stored as R objects just like anything
## else. In particular, they are R objects of class "function".

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseofx <- NULL
  set <- function(y) {
    x <<- y
    inverseofx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseofx <<- inverse
  getinverse <- function() inverseofx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseofx <- x$getinverse()
  if(!is.null(inverseofx)) {
    message("getting cached data")
    return(inverseofx)
  }
  data <- x$get()
  inverseofx <- inverse(data, ...)
  x$setinverse(inverseofx)
  inverseofx
}
