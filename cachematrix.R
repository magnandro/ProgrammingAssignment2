# COURSERA: R PROGRAMMING > WEEK 3 > ASSIGNMENT 2: LEXICAL SCOPING
# Alejandro Moreno Fresneda


## Using cache to enhance the performance in R

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) m <<- solve
    
    getsolve <- function() m
    
    list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
}


## CacheSolve computes the inverse of the special matrox returned by makeCacheMatrix. 
## If the inverse has been calculated before it retrieves the inverse from cache.


cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}





