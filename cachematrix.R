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