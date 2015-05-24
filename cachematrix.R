## Put comments here that give an overall description of what your
## functions do
## Maxtrix inversion is usually costly, especially when it has to compute repeatedly. 
## The following functionscompute and cache the inverse of a matrix.

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
##              1. set the value of the matrix
##              2. get the value of the matrix
##              3. set the inverse of the matrix
##              4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
        x <<- y
        inv <<- NULL
  }
  
  get = function() x
  
  setinverse = function(inverse) inv <<- inverse
  
  getinverse = function() inv
  
  ## return: a list containing functions
  list(set=set,
       get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
## The cacheSolve function returns the inverse of the matrix
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinverse()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  inv
}
