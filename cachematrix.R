## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse)  i <-- inverse
   getInverse <- function() i
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## The following function calculates the inverse of a special "matrix" created
## with the above function. It is important to clarify that the function first
## checks to see if the Inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation, otherwise, it calculates 
## the inverse of the given matrix and set the value of the inverse in the cache
## via the setInverse function. 

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   i <- x$getInverse()
   if(!is.null(i)){
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data)
   x$setInverse(i)
   i
}
