## A set of functions to create object to cache matrix inversion and to compute matrix inversion
## makeCacheMatrix - creates special "matrix" pbject
## cacheSolve - calculates inversion of th eobject created by makeCacheMatrix. 
##              This function will also cache the result and returns cached calculations
##              if the inverse has already been calculated (and the matrix has not changed)

# makeCacheMatrix creates a special "vector", which is really a list containing a function to
# set the value of the matric
# get the value of the matrix
# set the value of the inverted matrix
# get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve receive a special "cacheMatrix" object, and then returns 
# cached inverted matrix if it was cached or comuputes new one, caches it and returns the result

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}



