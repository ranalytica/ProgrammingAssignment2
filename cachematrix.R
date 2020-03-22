## This is a pair of function that cache the inverse of a matrix
## It creates a special matrix object that can cache its inverse

## 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<- y
                inv<<- NULL
        }
        get <- function() x
        set_Inverse <- function(inverse) inv <<- inverse
        get_Inverse <- function() inv
        list(set = set, get = get,
                set_Inverse = set_Inverse,
                get_Inverse = get_Inverse)

}


## cacheSolve function computes the inverse of the special matrix returned 
## by the above function - makeCacheMatrix. If the inverse is calculated and 
## matrix did not changed, then retrieve inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_Inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_Inverse(inv)
        inv
}
