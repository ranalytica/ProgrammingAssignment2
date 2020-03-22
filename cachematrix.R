## This is a pair of function that cache the inverse of a matrix
## It creates a special matrix object that can cache its inverse

## 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x<<- y
                m<<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
                setmean = setmean,
                getmean = getmean)

}


## cacheSolve function computes the inverse of the special matrix returned 
## by the above function - makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
