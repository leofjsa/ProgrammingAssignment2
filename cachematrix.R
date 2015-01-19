## FUNCTION FOR CALCULATING THE INVERSE OF A MATRIX, CACHING ITS VALUE

## How to use these functions:
## a <- makeCacheMatrix(x)    (where "x" is the matrix to be inverted and
##                             this function should be called only once or when "x" changes)
##
## b <- cacheSolve(a)         (this function should be called whenever the inverse of "x" is necessary)
##
## variable 'b' will contain the inverse of 'x'


## This function creates a list with 4 elements of class "function" 
## and a variable ("m") that keeps the cached value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInv <- function(mInv) m <<- mInv
      getInv <- function() m
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function checks if the value of the inversed matrix is already in cache
## If it is in cache, returns de value
## otherwise, calculates the value
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
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