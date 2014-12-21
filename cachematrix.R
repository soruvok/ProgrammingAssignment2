## Below is the exercise of speed optimisation
## of the calculation of the iverse matrix,
## which is heavily based on the "lexical scope"
## feature of the R language.
##
## The implementation is given in terms of the caching strategy,
## where the two helper functions are required:
##  makeCacheMatrix
##     (creates a "thing", which allows to manipulate the matrix
##      with a cached inverse)
## and
##  cacheSolve
##      (returns the inverse matrix)
##       The invertion itself, on the first call,
##       is done by solve() function in R.
##       On the second call, the cached value is returned,
##       without a re-calculation.

## In the implementation we assume that the matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        ## Description given in the problem definition (spec):
        ## Creates a special "matrix" object that can cache its inverse.

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_val){
    inv <<- inv_val
  }
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv , getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##
        ## Description given in the problem definition (spec):
        ## Computes the inverse of the special "matrix" returned
        ## by makeCacheMatrix above.
        ## If the inverse has already been calculated
        ## (and the matrix has not changed), then the cachesolve should
        ## retrieve the inverse from the cache

  inv <- x$getinv()
  if (!is.null(inv)){
    message("Getting cached value of inverse matrix:")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr,...)
  x$setinv(inv)
  message("Calculated the inverse matrix using the R's solve() function:")
  inv
}
