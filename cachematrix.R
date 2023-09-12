## Pair of functions that cache the inverse of a matrix
## Usage: Pass the result of a makeCacheMatrix call to cacheSolve 

#' Util function that set the matrix and the inverse in an environment
#' @param should be  an invertible matrix ( the matrix needs to be n x n, so we need to have the same ammount of rows and columns)

makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Compute and cache the inverse of a matrix
#' @param The resoult of the function call will be stored in the variable x
#' @param ... additional arguments to pass to solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}