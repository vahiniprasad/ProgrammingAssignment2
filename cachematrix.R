## Put comments here that give an overall description of what your
## functions do


## This function returns four functions set, get, set inverse and get inverse
## of given matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initializing inverse matrix to NULL
    inv <- NULL
    ## set function to cache matrix 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get function to retrive matrix from cache
    get <- function() x
    ## setinv function computes inverse matrix & caches it
    ## uses solve function to compute matrix inverse
    setinv <- function(solve) inv <<- solve
    ## getinv function retrives inverse matrix from cache
    getinv <- function() inv
    ## The Four functions of makeCacheMatrix
    list(set = set, get = get, setinv = setinv, 
         getinv = getinv)
}


## cacheSolve retrives or omputes & sets inverse matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## checks if inverse is already computed
    if(!is.null(inv)) {
        message("getting cached data")
        ## return the cached inverse matrix
        return(inv)
    }
    
    ## if inverse matrix in not already computed then get cached matrix 
    data <- x$get()
    ## compute the inverse matrix using solve function
    inv <- solve(data, ...)
    ## set the inverse matrix in cache 
    x$setinv(inv)
    inv
}
