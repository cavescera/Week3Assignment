## These are two functions to cache the inverse of a square matrix.

## This first function creates a matrix that caches its inverse.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix created above.
cacheSolve <- function(x, ...) {
  m  <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
 mm <- matrix(c(1, 2, 3, 5), 2, 2)
 mm1 <- makeCacheMatrix(mm)
 
 cacheSolve(mm1)
 solve(mm)
