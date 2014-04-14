## 'makeCacheMatrix (matrix x)' stores a given matrix x and allows the caching of its inverse 
## calculated and returned with 'cachesolve (makeCacheMatrix x)': cached inverse matrix returned when
## available, else computed by 'cacheSolve' and stored. Cached value is reset when matrix values are set.

## 'makeCacheMatrix (matrix)'
## .. creates a list type matrix which stores the matrix inverse
## .. $set() sets matrix, sets cached value to NULL
## .. $get() returns matrix
## .. $setinverse() sets 'inv' cached matrix inverse
## .. $getinverse() returns 'inv' cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # inv .. inverse holder
  inv <- NULL
  
  # set() .. store matrix, inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get() .. return matrix
  get <- function() x
  
  # setinv() .. set inverse
  setinverse <- function(inv_) inv <<- inv_
  
  # getinv() .. return inverse
  getinverse <- function() inv
  
  # list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function 'cacheSolve (makeCacheMatrix x)'
## .. returns a matrix that is the inverse of 'x' 
## .. returns cached value inv of x if existing, else solves for matrix x, 
## .. stores output in inv of x and returns inverse

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  }
  return(inv)
}
