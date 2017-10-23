## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## initialising special "matrix"
    inv <- NULL
    set <- function(y){
        ## (re-)setting the function
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    #setting the inverse of the matrix
    setinv <- function(solvematrix) inv <<- solvematrix
    #getting the inverse of the matrix
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above.If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()

    ##Checking for chached value  
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ##calculating inverse and caching it  
    mtrx <- x$get()
    inv <- solve(mtrx)
    x$setinv(inverse)
    inv
}