## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.
##  If matrix inverse is already calculated than matrix inverse from cache is
##  returned.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matInverse) m <<- matInverse
  getMatrix <- function() m
  list(set=set,get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and
##  the matrix has not changed), then the cachesolve should retrieve the
##  inverse from the cache. 

cacheSolve <- function(x, ...) {
  
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("Getting cached data matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}