## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmat <- function(mat) mat <<- matrix
  getmat <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,getmatrix=getmatrix)
}
