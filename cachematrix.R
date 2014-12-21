## Put comments here that give an overall description of what your
## functions do


## We assume that the matrix is always invertible.
## The invertion itself is done by solve() function in R.

## Write a short comment describing this function

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


## Write a short comment describing this function

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

