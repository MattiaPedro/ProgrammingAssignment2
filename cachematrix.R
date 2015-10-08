## This script defines two functions, makeCacheMatrix and cacheSolve, whose purpose is to cache the 
## time-consuming computation of the inverse of a Matrix. The first function, makeCacheMatrix, will
## create a special object, which intuitively is a matrix able to cache its own inverse. The second
## function will calculate the inverse of the special matrices, but after checking if the computation
## has already been done, and its result cached.

## The function makeCacheMatrix takes a matrix as input and builds a special "object" that stores both
## the matrix and its inverse. This object is actually a list of four functions (setMatrix, getMatrix,
## setInverse, getInverse) which respectively set and get the value of the matrix and set and get the 
## inverse of the matrix. The function make use of the `<<-` operator to assign a value to an object 
## in an environment that is different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## The following function cacheSolve calculates the inverse of the special matrix created with the
## previous function. It takes as input a 'matrix' of the special type defined above, and an optional
## list of parameters to pass to the 'solve' function. First it uses the function 'getInverse' to
## check if the inverse has already been calculated and cached, and in this case skips the computation.
## If not, then calls the 'getMatrix' function, compute the inverse, and use 'setInverse' to cache 
## the computed value.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  inv <- solve(x$getMatrix(), ...)
  x$setInverse(inv)
  
  inv
}

## Test run: to check how faster the computation of the inverse is in this way, we created a
## 3000 x 3000 matrix, computed its inverse one and then again with cacheSolve, with the following
## code:
## 
## A = matrix(rnorm(9000000), nrow = 3000)
## Amat <- makeCacheMatrix(A)
## system.time(cacheSolve(Amat))
## system.time(cacheSolve(Amat))
## 
## The first call of cacheSolve, in which the inverse was actually computed, took 51 seconds, while
## the second call, when the inverse was taken from the cache, took 0 seconds.

