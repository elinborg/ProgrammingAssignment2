## The following two functions cache the inverse of a invertible square matrix

## This is a function that provides functions that set and get
## the value of an invertible square matrix x.
## In addition, it provides functions setinv and getinv, that set and get the value of the inverse of x
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invx) inv <<- invx
  getinv <- function() inv
  list(set = set,get = get,setinv = setinv, getinv = getinv)
}


## This is a function that calculates the inverse of the invertible square matrix x.
## The inverse is only calculated for the first time that this function is called for the matrix x.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting cache data for the inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
