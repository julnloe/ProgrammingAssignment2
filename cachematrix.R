makeCacheMatrix <- function(x = matrix()) {
  in_verse <- NULL
  set <- function(y) {
    x <<- y
    in_verse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) in_verse <<- inverse
  getinverse <- function() in_verse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(in_verse)) {
    message("getting cached data.")
    return(in_verse)
  }
  data <- x$get()
  in_verse <- solve(data)
  x$setinverse(in_verse)
  in_verse
}