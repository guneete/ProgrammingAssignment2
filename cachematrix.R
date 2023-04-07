## Following are two functions that have been used to create a special object that 
## stores a square matrix and also caches its inverse.

## first function, makeCacheMatrix creates a matrix, 
## which is really a list containing a function to:
## set elements of the matrix
## get elements of the matrix
## set elements of the matrix inverse
## get elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
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


## 2nd function will calculate the inverse of the matrix created with the above function. 
## It will first check to see if the inverse was previously calculated.
## If Yes, it will take inverse from cache to skip computation. 
## If Not, it will calculate the inverse of the matrix. 
## and will set it in cache using setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("obtaining cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
