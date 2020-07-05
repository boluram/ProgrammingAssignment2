## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned my makeCacheMatrix above.
##If the matrix has already calculated and the matrix has not changed, then it will retrive the inverse from the cache.


cacheSolve <- function(x, ...) {     ## Return a matrix that is the inverse of 'x'
       inv <- x$getInverse()
       if(is.null(inv)){
         print("cached data")
         return(inv)
       }
       mat <- x$get()
       inv <- solve(mat,...)
       x$setInverse()
       inv
  
}
