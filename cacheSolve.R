## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getmatrix()
  if(!is.null(mat)) {
    message("getting cached matrix data")
    return(mat)
    data <- x$get()
    mat <- solve(data, ...)
    x$setmatrix(mat)
    mat
  }
}  
