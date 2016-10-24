
    #makeCacheMatrix(): function to create a special matrix object that is caching its inverse.

    #cacheSolve(): function that computes inverse of the matrix returned by makeCacheMatrix(). If cache exists, directly retrieved and not computed again.




makeCacheMatrix <- function(x = matrix()) {

    # x: invertible matrix
    # returning: functions to
    #              1. set the matrix
    #              2. get the matrix
    #              3. set the inverse
    #              4. get the inverse
    #         this list is used as the input to cacheSolve()
        
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


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv = x$getinv()
    
    # if inverse exists, get it from the cache 
    if (!is.null(inv)){
            message("getting cached data")
            return(inv)
    }
    
    # otherwise, compute inverse 
    data = x$get()
    inv = solve(data, ...)
    
    x$setinv(inv)
    
    return(inv)
}
