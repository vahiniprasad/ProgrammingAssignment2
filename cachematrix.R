## Put comments here that give an overall description of what your
## functions do


## This function comprises  four functions set  matrix,get  matrix,set  inverse of  matrix, 
## get inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initializing inverse matrix to NULL
    inverse_x <- NULL
    ## set function to cache matrix 
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    ## get function to retrive matrix from cache
    get <- function() x
    ## setinv function computes inverse matrix & caches it
    ## uses solve function to compute matrix inverse
    setinv <- function(solve) inverse_x <<- solve
    ## getinv function to retrive inverse matrix from cache
    getinv <- function() inverse_x
    ## The Four functions of makeCacheMatrix
    list(set = set, get = get, setinv = setinv, 
         getinv = getinv)
}


## cacheSolve computes, retrives and sets inverse matrix


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## checks if inverse is already computed
    if(!is.null(inverse_x)) {
        message("getting cached data")
        ## return the cached inverse matrix
        return(inverse_x)
    }
    
    ## if inverse matrix in not already computed then
    ## get the cached matrix 
    data <- x$get()
    ## compute the inverse matrix using solve function
    inverse_x <- solve(data, ...)
    ## set the inverse matrix in cache 
    x$setinv(inverse_x)
    inverse_x
}
