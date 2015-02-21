## These two functions will look to see if the test matrix inverse has been calculated (it will be
## in the cache if it has) and if not it will calculate it and cache it via the setMat function.
## 

## This function creates an object that can cache the inverse of the test matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setMat <- function(solve) {
    m <<- solve
  }
  getMat<-function() {
    m
  }
  list(set=set, get=get, setMat=setMat, getMat=getMat)
}


## This function will look to see if the inverse has been calculated and if it has been it will get
## the answer from the cached object created in makeCacheMatrix.  If it has not it will calculate 
## the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMat() ## Sets the cached matrix object to m
  ## Looks to see if the cache is null or not.  If it is not then it returns the cache value.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Otherwise it calculates the inverse and sets the cache to equal the new answer.
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setMat(m)
  return(m)
}
