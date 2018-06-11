## This two functions can be used to calculate the inverse of a matrix or to return the inverse of a matrix previously calculated

## makeCacheMatrix return a list containing 4 functions (set, get, setSolve and getSolve). These arguments will be used by the cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
    
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(inverse) inv <<- inverse
    getSolve <- function() inv
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
    
}


## The cacheSolve function will first check if the inverse matrix has been previously calculated. If yes, the inverse matrix stored in the memory will be return with the message "getting catched data". If not, the inverse matrix will be calcultaed and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getSolve()  ## to extract the argument called "getSolve()" from the list return by makeCacheMatrix
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }				## If m is not "NULL", 
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolve(inv)
    inv
    
}

