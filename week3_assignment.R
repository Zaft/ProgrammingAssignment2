## The following two functions are used to calculate the inverse of a matrix. 
## As this can be a costly operation, the 'makeCacheMatrix' function provides
## caching of the result of the inverse computation.

## This function create a special 'matrix' object that is a list containing a function to
## 1.set the value of a matrix
## 2.get the value of a matrix
## 3.set the value of the inverse of a matrix
## 4.get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the special matrix created
## with the above function. It first checks to see if the inverse has already
## been calculated. If so, it retrieves the value from cache and skips the 
## computation. Otherwise it calculates the inverse of the matrix by using the
## setInverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) { ##get the cached data
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
