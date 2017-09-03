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

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(in_verse)) {
    message("loading cached data.")
    return(in_verse)
  }
  data <- x$get()
  in_verse <- solve(data)
  x$setinverse(in_verse)
  in_verse
}