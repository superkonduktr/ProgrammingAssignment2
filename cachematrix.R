## A cache matrix implementation ヽ(• ‿ •)ノ

## Accepts a matrix and returns a 'cache matrix' that can store the
## result of it being solved.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  getsolved <- function() s
  setsolved <- function(solved) s <<- solved
  list(set = set,
       get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}

## Accepts a 'cache matrix' returned by makeCacheMatrix and solves it.
## After the first call, the result of the computation is cached and
## returned on all subsequent calls.

cacheSolve <- function(x, ...) {
  s <- x$getsolved()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolved(s)
  s
}
