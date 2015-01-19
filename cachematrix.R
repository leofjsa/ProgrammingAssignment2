## FUNCTION FOR CALCULATING THE INVERSE OF A MATRIX AND CACHING ITS VALUE

## These functions were created based on the example given in the 'programming assignment 2',
## substituting the 'mean' calculations for a 'inverse matrix' calculation
##
## These functions must be used in 2 steps:
## a <- makeCacheMatrix(x)    (where "x" is the matrix to be inverted) 
##                            (this function should be called only once or when "x" changes)
##
## b <- cacheSolve(a)         (this function should be called whenever the inverse of "x" is necessary)
##
## Expected result: variable 'b' must contain the inverse of matrix 'x'

# Function makeCacheMatrix cretes an environment and a  variable 'm' that keeps the cached value of the inversed matrix
# and returns a list with 4 elements of class 'function', which are used in the cacheSolve function
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
## otherwise, first calculates and then returns the value of the inversed matrix
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