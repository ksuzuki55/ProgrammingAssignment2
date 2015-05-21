## Caching the Inverse of a Matrix 

## 1. makeCacheMatrix:
##      This function creates a special "matrix" object that can
##      cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## 1. set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## 2. get the value of the matrix
        get <- function() x
        
        ## 3. set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        
        ## 4. get the value of the inverse of the matrix
        getsolve <- function() m
        
        # return list of function
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## 2. cacheSolve:
##      This function computes the inverse of the special "matrix"
##      returned by makeCacheMatrix above. If the inverse has
##      already been calculated (and the matrix has not changed),
##      then the cachesolve should retrieve the inverse from the
##      cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        ## if the inverse has already been calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if the inverse has not been calculated yet
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
