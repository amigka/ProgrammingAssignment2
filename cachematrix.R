
##This function creates a matrix that can cache its inverse
##Input x is the matrix
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting inversed matrix")
    return(j)
  }
  data <- x$get()
  j <- solve(data,...)
  x$setInverse(j)
  j
}