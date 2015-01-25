## The following is to get the inverse of a invertible square matrix and cache it. 
## makeCacheMatrix creates that special matrix that can be cached. 
## solveCache helps get the cached inverse of the matrix.

## Write a short comment describing this function

## Write a short comment describing this function
#makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of matrix
#4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#The following function returns the inverse of a square matrix from the cache, if already available
#Or if there is no cache, calculates and returns it. Also, stores the calculated value in cache.
#The input needs to be a invertible square matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}