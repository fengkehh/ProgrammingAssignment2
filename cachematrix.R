## makeCacheMatrix constructs a special matrix object that can store its 
## computed inverse.

## ARGUMENT:
## x: INVERTIBLE numeric matrix
## 
## RETURNS:
## result: list that represents the modified matrix object. Containing the 
## following subroutines:
## 
## result$set(x): sets the matrix x and stores it
## 
## result$get(): returns the matrix x
## 
## result$setinv(x_inv): sets the cached matrix inverse for x to x_inv and 
## stores it
## 
## result$getinv(): gets the cahced matrix inverse x^(-1) or returns NULL if it 
## hasn't been set yet.
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the inverse to NULL first
    x_inv <- NULL
    
    # define the "set" subroutine to set stored numeric matrix x.  Notice that 
    # calling set resets x_inv to NULL because inverse may have to be recomputed.
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    # define the "get" subroutine to retrieve stored numeric matrix x.
    get <- function() x
    
    # define the "setinv" subroutine to set and store matrix inverse
    setinv <- function(inv) x_inv <<- inv
    
    # define the "getinv" subroutine to retrieve cached inverse (not necessarily
    # computed yet!)
    getinv <- function() x_inv
    
    # return the list with the defined subroutines
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special "matrix" object such as 
## that made by makeCacheMatrix.  If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

## ARGUMENT:
## x: special CacheMatrix object (see makeCacheMatrix)
cacheSolve <- function(x, ...) {
    
    ## Retrieve the currently cached matrix inverse.
    x_inv <- x$getinv()
    
    if (!is.null(x_inv)) {
        # Cached inverse found! Returning it.
        message("getting cached data")
        return(x_inv)
    }
    
    # Cached inverse not found. Compute, set and return inv.
    x_mat <- x$get()
    x_inv <- solve(x_mat, ...)
    x$setinv(x_inv)
    
    # return inv
    x_inv
    
}
