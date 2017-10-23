## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solvematrix) inv <<- solvematrix
  getinv <- function() inv
# list with names => allows subsetting by name  
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above.If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()

#Checking for chached value  
    if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
#calculating inverse and caching it  
  mtrx <- x$get()
  inv <- solve(mtrx)
  x$setinv(inv)
  inv
}