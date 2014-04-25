# makeCacheMatrix function creates the matrix
# and getter and setter methods for it
# and for the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(m) m <<- m
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}

# cacheSolve function checks if Inv matrix exists in cache
# if it does, it returns it
# if it can't be found, it is solved, cached and returned

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}