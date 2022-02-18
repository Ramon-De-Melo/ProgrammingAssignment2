## The following two functions cache and obtain the inverse of 
## matrices, respectively.

## This function caches the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## This function obtains the inverse of the cached matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}

## Example
## The same result is obtained using the solve() function from base R, and 
## and the makeCacheMatrix and cacheSolve

m <- matrix(rnorm(4), ncol = 2, nrow = 2) # Create matrix

solve(m) # obtain the inverse using solve()

p <- makeCacheMatrix(m); cacheSolve(p) # obtain the inverse using functions

