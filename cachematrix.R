## Caching the Inverse of a Matrix

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setslv <- function(slv) s <<- slv
  getslv <- function() s
  list(set = set, get = get,
       setslv = setslv,
       getslv = getslv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getslv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setslv(s)
  s
}