## Put comments here that give an overall description of what your
## functions do

## Function “makeCacheMatrix” creates a special "matrix" object
    that can cache its inverse. "makeCacheMatrix" contains 4 functions: 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
      iv <- NULL
      set <- function(y) {
            x <<- y
            iv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) iv <<- solve
      getinverse <- function() iv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Function “cacheSolve” computes the inverse of the special “matrix” 
(which is the input of cachemean) returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      iv <- x$getinverse()
      if(!is.null(iv)) {
            message("getting cached data")
            return(iv)
      }
      data <- x$get()
      iv <- solve(data, ...)
      x$setinverse(iv)
      iv
}
