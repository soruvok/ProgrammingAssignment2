m <- matrix(
  c(3,2,7,4,5,6,7,8,9),
  nrow=3,
  ncol=3)

cm <- makeCacheMatrix(m)
cacheSolve(cm)
cacheSolve(cm)

