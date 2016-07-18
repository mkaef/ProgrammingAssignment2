## The objective of this assignment is to write a pair of functions that cache the 
## inverse of a matrix.Matrix inversion is usually a costly computation and there
## may be some benefit o caching the inverse of a matrix rather than compute it 
## repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
   set <- function(y) {
   x <<- y
   m <<- NULL
   
  }
  get <- function() x
  setm <- function(inverse) m <<- inverse 
  getm <- function() m
  list(set=set, get=get, setm=setm, getm=getm)
}



## cacheSolvecomputes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  m <- x$getm()
  
  if (!is.null(m)){
    message("getting cached data")
    m
  }
  
  mat.data <- x$get()
  m <- solve(mat.data, ...)
  
  x$setm(m)
  m
}    

