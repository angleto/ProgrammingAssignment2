## Cache the inverse of a matrix through a wrapper
## Usage Sample:
## create a square matrix
##   mat<-matrix(1:9,3,3)
##   mat[1,2]<-1
## create the wrapped object
##   wrappedM <- makeCacheMatrix(mat)
## get the matrix
##   wrappedM$get()
## calculated the inverse and cache the result
##   cacheSolve(wrappedM)
## call again the cacheSolve in order to get the inverse matrix
##  this time the value is retrived from cache
##   cacheSolve(wrappedM)
## get the inverse matrix
##   wrappedM$getinv()

## create a wrapped matrix  object with an empty matrix or with the matrix passed as argument
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#calculate the inverse of the matrix and cache the object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
