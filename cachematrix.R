## The two functions below will create a special matrix object,
## generate the inverse, store it to cache, check if the inverse has
## been calculated already, and if so, retrieve the inverse from cache


## This first function will create a special matrix object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This second function computes the inverse of the special matrix 
## returned by the first function above. However, if the inverse has
## been calculated already, and there are no changes to the matrix,
## this function should retrieve the inverse from the cache. 

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
